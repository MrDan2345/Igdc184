unit Audio;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$optimization autoinline}
{$macro on}
{$warn 6058 off}
{$warn 5024 off}
{$warn 3123 off}
{$warn 3124 off}
{$WARN 5026 off}
{$WARN 6018 off}

interface

uses
  Externals,
  Utils;
  //CommonUtils;
  //MMSystem;

type TMidiStreamEvent = packed record
  DeltaTime: UInt32;
  StreamID: UInt32;
  Event: UInt32;
  class operator > (const a, b: TMidiStreamEvent): Boolean;
end;

type TMidiFile = record
public
  type TTrackEvent = record
    AbsTime: UInt32;
    Event: UInt32;
    Patch: UInt8;
    class operator > (const a, b: TTrackEvent): Boolean;
  end;
public
  var Ticks: UInt16;
  var Stream: array of TMidiStreamEvent;
  procedure Load(const Data: Pointer; const Size: UInt32);
end;

type TAudio = record
private
  var _Handle: THandle;
  var _Stream: array of TMidiStreamEvent;
  var _StreamPos: Int32;
  var _StreamTime: Int32;
  var _CurPatch: UInt8;
public
  procedure Initialize;
  procedure Finalize;
  procedure Update(const dt: UInt32);
  procedure PlayMidi(const MidiFile: TMidiFile);
  procedure PlaySound(const Patch: UInt8; const Note: UInt8; const Volume: UInt8 = 60);
end;

implementation

class operator TMidiStreamEvent.>(const a, b: TMidiStreamEvent): Boolean;
begin
  Result := a.DeltaTime > b.DeltaTime;
end;

procedure TMidiFile.Load(const Data: Pointer; const Size: UInt32);
  type THeader = packed record
    Id: array[0..3] of AnsiChar;
    Size: UInt32;
    Format: UInt16;
    Tracks: UInt16;
    Ticks: UInt16;
  end;
  type TTrack = array of TTrackEvent;
  var sh: TStreamHelper;
  function ReadVarLen(out ByteCount: UInt8): UInt32;
    var b: UInt8;
  begin
    Result := 0;
    ByteCount := 0;
    repeat
      b := sh.ReadUInt8;
      Inc(ByteCount);
      Result := (Result shl 7) + (b and $7f);
    until b and $80 = 0;
  end;
  function ReadTrack: TTrack;
    type TTrackHeader = packed record
      Id: array[0..3] of AnsiChar;
      Size: UInt32;
    end;
    var Header: TTrackHeader;
    var Track: TTrack;
    var Event: TTrackEvent;
    var bc: UInt8;
    var Cmd, Msg, Last: UInt32;
    var FinalLength: UInt32;
    var AbsTime: UInt32;
    var CurPatch: UInt8;
  begin
    sh.ReadBuffer(@Header, SizeOf(TTrackHeader));
    Header.Size := UEndianSwap(Header.Size);
    FinalLength := sh.Position + Header.Size;
    Track := nil;
    Last := 0;
    AbsTime := 0;
    CurPatch := 0;
    repeat
      Event := Default(TTrackEvent);
      AbsTime += ReadVarLen(bc);
      Event.AbsTime := AbsTime;
      Cmd := sh.ReadUInt8;
      if Cmd = $ff then
      begin
        Cmd := sh.ReadUInt8;
        Cmd := sh.ReadUInt8;
        sh.Skip(Cmd);
      end
      else
      begin
        if Cmd and $80 = 0 then Cmd := Last;
        if (Cmd and $f0) <> $f0 then
        begin
          Last := Cmd;
          Msg := sh.ReadUInt8;
          Event.Event := Cmd or (Msg shl 8);
	  if not (((Cmd and $f0) = $c0) or ((Cmd and $f0) = $d0)) then
	  begin
            Msg := sh.ReadUInt8;
	    Event.Event := Event.Event or (Msg shl 16);
	  end;
          if (Event.Event and $c0) = $c0 then
          begin
            CurPatch := (Event.Event shr 8) and $7f;
          end
          else
          begin
            Event.Patch := CurPatch;
            specialize UArrAppend<TTrackEvent>(Track, Event);
          end;
        end;
      end;
    until sh.Position >= FinalLength;
    Result := Track;
  end;
  var Header: THeader;
  var Tracks: array of TTrack;
  var Track: TTrack;
  var Mix: TTrack;
  var Event: TMidiStreamEvent;
  var CurPatch: UInt8;
  var AbsTime, DeltaTime: UInt32;
  var i, j, n: Int32;
begin
  sh := TStreamHelper.Create(Data, Size);
  try
    sh.ReadBuffer(@Header, SizeOf(Header));
    Header.Size := UEndianSwap(Header.Size);
    Header.Format := UEndianSwap(Header.Format);
    Header.Tracks := UEndianSwap(Header.Tracks);
    Header.Ticks := UEndianSwap(Header.Ticks);
    Ticks := Header.Ticks;
    Tracks := nil;
    for i := 0 to Header.Tracks - 1 do
    begin
      Track := ReadTrack;
      if Length(Track) = 0 then Continue;
      specialize UArrAppend<TTrack>(Tracks, Track);
    end;
  finally
    sh.Free;
  end;
  n := 0;
  for i := 0 to High(Tracks) do n += Length(Tracks[i]);
  Mix := nil;
  SetLength(Mix, n);
  n := 0;
  for i := 0 to High(Tracks) do
  for j := 0 to High(Tracks[i]) do
  begin
    Mix[n] := Tracks[i][j];
    Inc(n);
  end;
  specialize UArrSort<TTrackEvent>(Mix);
  CurPatch := 0;
  Event := Default(TMidiStreamEvent);
  Event.StreamID := 0;
  AbsTime := 0;
  Stream := nil;
  for i := 0 to High(Mix) do
  begin
    DeltaTime := Mix[i].AbsTime - AbsTime;
    Event.DeltaTime := DeltaTime;
    AbsTime := Mix[i].AbsTime;
    if Mix[i].Patch <> CurPatch then
    begin
      CurPatch := Mix[i].Patch;
      Event.Event := $c0 or (CurPatch shl 8);
      specialize UArrAppend<TMidiStreamEvent>(Stream, Event);
      Event.DeltaTime := 0;
    end;
    Event.Event := Mix[i].Event;
    specialize UArrAppend<TMidiStreamEvent>(Stream, Event);
  end;
end;

class operator TMidiFile.TTrackEvent.>(const a, b: TTrackEvent): Boolean;
begin
  Result := a.AbsTime > b.AbsTime;
end;

procedure TAudio.Initialize;
begin
  midiOutOpen(@_Handle, WAVE_MAPPER, 0, 0, 0);
  midiOutReset(_Handle);
  _CurPatch := 0;
end;

procedure TAudio.Finalize;
begin
  if _Handle > 0 then midiOutClose(_Handle);
end;

procedure TAudio.Update(const dt: UInt32);
begin
  if Length(_Stream) = 0 then Exit;
  _StreamTime += dt;
  if _StreamPos > High(_Stream) then
  begin
    _StreamPos := 0;
    _StreamTime := 0;
  end;
  while _StreamPos < Length(_Stream) do
  begin
    if _Stream[_StreamPos].DeltaTime > _StreamTime then Break;
    _StreamTime -= _Stream[_StreamPos].DeltaTime;
    midiOutShortMsg(_Handle, _Stream[_StreamPos].Event);
    if (_Stream[_StreamPos].Event and $c0) = $c0 then
    begin
      _CurPatch := (_Stream[_StreamPos].Event shr 8) and $7f;
    end;
    Inc(_StreamPos);
  end;
end;

procedure TAudio.PlayMidi(const MidiFile: TMidiFile);
begin
  _StreamPos := 0;
  _StreamTime := 0;
  SetLength(_Stream, Length(MidiFile.Stream));
  Move(MidiFile.Stream[0], _Stream[0], SizeOf(MidiFile.Stream[0]) * Length(MidiFile.Stream));
end;

procedure TAudio.PlaySound(const Patch: UInt8; const Note: UInt8; const Volume: UInt8);
begin
  if _CurPatch <> Patch then
  begin
    midiOutShortMsg(_Handle, $c0 or (Patch shl 8));
  end;
  midiOutShortMsg(_Handle, $90 or (Note shl 8) or (Volume shl 16));
  if _CurPatch <> Patch then
  begin
    midiOutShortMsg(_Handle, $c0 or (_CurPatch shl 8));
  end;
end;

end.

