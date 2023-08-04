unit Utils;

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
  Externals;
  //Windows, gl;

type TUFloat = Single;
type TUVec2 = array[0..1] of TUFloat;
type PUVec2 = ^TUVec2;
type TUVec3 = array[0..2] of TUFloat;
type PUVec3 = ^TUVec3;
type TUVec4 = array[0..3] of TUFloat;
type PUVec4 = ^TUVec4;
type TUMat = array[0..3, 0..3] of TUFloat;
type PUMat = ^TUMat;
type TUColor = UInt32;
type PUColor = ^TUColor;

type TUVec2Impl = type helper for TUVec2
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  class function Zero: TUVec2; static; inline;
  class function Make(const Ax, Ay: TUFloat): TUVec2; static; overload; inline;
  class function Make(const S: TUFloat): TUVec2; static; overload; inline;
  class function Dot(const v0, v1: TUVec2): TUFloat; static; overload; inline;
  class function Norm(const v: TUVec2): TUVec2; static; overload; inline;
  class function Len(const v: TUVec2): TUFloat; static; overload; inline;
  function Dot(const v: TUVec2): TUFloat; overload; inline;
  function Norm: TUVec2; overload; inline;
  function Len: TUFloat; overload; inline;
end;

type TUVec3Impl = type helper for TUVec3
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
  function GetZ: TUFloat; inline;
  procedure SetZ(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  property z: TUFloat read GetZ write SetZ;
  class function Zero: TUVec3; static; inline;
  class function Make(const Ax, Ay, Az: TUFloat): TUVec3; static; overload; inline;
  class function Make(const v2: TUVec2; const Az: TUFloat): TUVec3; static; overload; inline;
  class function Make(const s: TUFloat): TUVec3; static; overload;
end;

type TUVec4Impl = type helper for TUVec4
private
  function GetX: TUFloat; inline;
  procedure SetX(const Value: TUFloat); inline;
  function GetY: TUFloat; inline;
  procedure SetY(const Value: TUFloat); inline;
  function GetZ: TUFloat; inline;
  procedure SetZ(const Value: TUFloat); inline;
  function GetW: TUFloat; inline;
  procedure SetW(const Value: TUFloat); inline;
public
  property x: TUFloat read GetX write SetX;
  property y: TUFloat read GetY write SetY;
  property z: TUFloat read GetZ write SetZ;
  property w: TUFloat read GetW write SetW;
  class function Zero: TUVec4; static; inline;
  class function Make(const Ax, Ay, Az, Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const v: TUVec3; const Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const v0, v1: TUVec2): TUVec4; static; overload; inline;
  class function Make(const v: TUVec2; const Az, Aw: TUFloat): TUVec4; static; overload; inline;
  class function Make(const s: TUFloat): TUVec4; static; overload; inline;
end;

type TUMatImpl = type helper for TUMat
private
  function GetElement(const Index: UInt32): TUFloat; inline;
  procedure SetElement(const Index: UInt32; const Value: TUFloat); inline;
public
  property Element[const Index: UInt32]: TUFloat read GetElement write SetElement; default;
  class function Make(
    const e00, e10, e20, e30: TUFloat;
    const e01, e11, e21, e31: TUFloat;
    const e02, e12, e22, e32: TUFloat;
    const e03, e13, e23, e33: TUFloat
  ): TUMat; static;
  class function Zero: TUMat; static; inline;
  class function Identity: TUMat; static; inline;
end;

type TUColorImpl = type helper for TUColor
private
  function GetChannel(const Index: UInt8): UInt8; inline;
  procedure SetChannel(const Index: UInt8; const Value: UInt8); inline;
  function GetR: UInt8; inline;
  procedure SetR(const Value: UInt8); inline;
  function GetG: UInt8; inline;
  procedure SetG(const Value: UInt8); inline;
  function GetB: UInt8; inline;
  procedure SetB(const Value: UInt8); inline;
  function GetA: UInt8; inline;
  procedure SetA(const Value: UInt8); inline;
public
  property Channel[const Index: UInt8]: UInt8 read GetChannel write SetChannel; default;
  property r: UInt8 read GetR write SetR;
  property g: UInt8 read GetG write SetG;
  property b: UInt8 read GetB write SetB;
  property a: UInt8 read GetA write SetA;
  class function Make(const Ar, Ag, Ab: UInt8; const Aa: UInt8 = $ff): TUColor; static;
  function Inverse: TUColor;
  function AsVec4: TUVec4;
  function AsVec3: TUVec3;
end;

type TStreamHelper = class
private
  var _Ptr: Pointer;
  var _Pos: UInt32;
  var _Size: UInt32;
public
  property Position: UInt32 read _Pos;
  constructor Create(const Buffer: Pointer; const BufferSize: UInt32);
  destructor Destroy; override;
  generic function Read<T>(): T;
  procedure ReadBuffer(const Buffer: Pointer; const BufferSize: UInt32);
  function ReadUInt8: UInt8;
  procedure Skip(const Size: UInt32);
end;

type TFont = record
public
  var CharProps: array[UInt8] of record
    Width: Int32;
    Height: Int32;
    OffsetX: Int32;
    OffsetY: Int32;
  end;
  var CharSpaceX: Int32;
  var CharSpaceY: Int32;
  var TextureId: GLuint;
  var TexWidth: Int32;
  var TexHeight: Int32;
  var Size: Int32;
  var Face: String;
  procedure Initialize(const FontFace: String = 'Arial'; const Bold: Boolean = False);
  procedure Finalize;
  procedure Print(const Pos: TUVec2; const Scale: TUVec2; const Text: String; const Color: TUColor);
  function TextWidth(const Text: String): Int32;
  function TextHeight(const Text: String): Int32;
end;

generic TUArray<T> = array of T;

const UPi = 3.14159265359;
const UTwoPi = UPi * 2;
const UHalfPi = UPi * 0.5;
const URcp255 = 1 / 255;
const UEps = 1E-5;
const UDegToRad = UPi / 180;
const URadToDeg = 180 / UPi;

function UClamp(const v, MinV, MaxV: Int8): Int8; inline; overload;
function UClamp(const v, MinV, MaxV: Int16): Int16; inline; overload;
function UClamp(const v, MinV, MaxV: Int32): Int32; inline; overload;
function UClamp(const v, MinV, MaxV: Int64): Int64; inline; overload;
function UClamp(const v, MinV, MaxV: UInt8): UInt8; inline; overload;
function UClamp(const v, MinV, MaxV: UInt16): UInt16; inline; overload;
function UClamp(const v, MinV, MaxV: UInt32): UInt32; inline; overload;
function UClamp(const v, MinV, MaxV: UInt64): UInt64; inline; overload;
function UClamp(const v, MinV, MaxV: TUFloat): TUFloat; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec2): TUVec2; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec3): TUVec3; inline; overload;
function UClamp(const v, MinV, MaxV: TUVec4): TUVec4; inline; overload;
generic function UEndianSwap<T>(const v: T): T; inline; overload;
function UEndianSwap(const v: UInt16): UInt16; inline; overload;
function UEndianSwap(const v: UInt32): UInt32; inline; overload;
procedure USinCos(const a: TUFloat; out s: TUFloat; out c: TUFloat);
function UIntToStr(const v: Int32): String;
generic procedure UArrSort<T>(var Arr: array of T);
generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Item: T); overload;

operator + (const v0, v1: TUVec2): TUVec2;
operator - (const v0, v1: TUVec2): TUVec2;
operator * (const v0, v1: TUVec2): TUVec2;
operator / (const v0, v1: TUVec2): TUVec2;
operator * (const v: TUVec2; const f: TUFloat): TUVec2;
operator * (const f: TUFloat; const v: TUVec2): TUVec2;
operator - (const v: TUVec2): TUVec2;
operator + (const v0, v1: TUVec3): TUVec3;
operator - (const v0, v1: TUVec3): TUVec3;
operator * (const v0, v1: TUVec3): TUVec3;
operator / (const v0, v1: TUVec3): TUVec3;
operator * (const v: TUVec3; const f: TUFloat): TUVec3;
operator * (const f: TUFloat; const v: TUVec3): TUVec3;
operator - (const v: TUVec3): TUVec3;
operator + (const v0, v1: TUVec4): TUVec4;
operator - (const v0, v1: TUVec4): TUVec4;
operator * (const v0, v1: TUVec4): TUVec4;
operator / (const v0, v1: TUVec4): TUVec4;
operator * (const v: TUVec4; const f: TUFloat): TUVec4;
operator * (const f: TUFloat; const v: TUVec4): TUVec4;
operator - (const v: TUVec4): TUVec4;
operator mod (const a, b: TUFloat): TUFloat;

procedure CheckGLErrors;

implementation

procedure CheckGLErrors;
  var Err: GLenum;
begin
  repeat
    Err := glGetError();
    if (Err <> GL_NO_ERROR) then
    begin
      WriteLn('GL Error: ', Err);
    end;
  until Err = GL_NO_ERROR;
end;

function TUVec2Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec2Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec2Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec2Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

class function TUVec2Impl.Zero: TUVec2;
begin
  Result := Make(0);
end;

class function TUVec2Impl.Make(const Ax, Ay: TUFloat): TUVec2;
begin
  Result[0] := Ax;
  Result[1] := Ay;
end;

class function TUVec2Impl.Make(const S: TUFloat): TUVec2;
begin
  Result := Make(s, s);
end;

class function TUVec2Impl.Dot(const v0, v1: TUVec2): TUFloat;
begin
  Result := v0[0] * v1[0] + v0[1] * v1[1];
end;

class function TUVec2Impl.Norm(const v: TUVec2): TUVec2;
  var d: TUFloat;
begin
  d := Sqrt(Dot(v, v));
  if d > 0 then
  begin
    d := 1 / d;
    Result := Make(v.x * d, v.y * d);
  end
  else
  begin
    Result := Zero;
  end;
end;

class function TUVec2Impl.Len(const v: TUVec2): TUFloat;
begin
  Result := Sqrt(Sqr(v.x) + Sqr(v.y));
end;

function TUVec2Impl.Dot(const v: TUVec2): TUFloat;
begin
  Result := Dot(Self, v);
end;

function TUVec2Impl.Norm: TUVec2;
begin
  Result := Norm(Self);
end;

function TUVec2Impl.Len: TUFloat;
begin
  Result := Len(Self);
end;

function TUVec3Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec3Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec3Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec3Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

function TUVec3Impl.GetZ: TUFloat;
begin
  Result := Self[2];
end;

procedure TUVec3Impl.SetZ(const Value: TUFloat);
begin
  Self[2] := Value;
end;

class function TUVec3Impl.Zero: TUVec3;
begin
  Result := Make(0);
end;

class function TUVec3Impl.Make(const Ax, Ay, Az: TUFloat): TUVec3;
begin
  Result[0] := Ax;
  Result[1] := Ay;
  Result[2] := Az;
end;

class function TUVec3Impl.Make(const v2: TUVec2; const Az: TUFloat): TUVec3;
begin
  Result := Make(v2[0], v2[1], Az);
end;

class function TUVec3Impl.Make(const s: TUFloat): TUVec3;
begin
  Result := Make(s, s, s);
end;

function TUVec4Impl.GetX: TUFloat;
begin
  Result := Self[0];
end;

procedure TUVec4Impl.SetX(const Value: TUFloat);
begin
  Self[0] := Value;
end;

function TUVec4Impl.GetY: TUFloat;
begin
  Result := Self[1];
end;

procedure TUVec4Impl.SetY(const Value: TUFloat);
begin
  Self[1] := Value;
end;

function TUVec4Impl.GetZ: TUFloat;
begin
  Result := Self[2];
end;

procedure TUVec4Impl.SetZ(const Value: TUFloat);
begin
  Self[2] := Value;
end;

function TUVec4Impl.GetW: TUFloat;
begin
  Result := Self[3];
end;

procedure TUVec4Impl.SetW(const Value: TUFloat);
begin
  Self[3] := Value;
end;

class function TUVec4Impl.Zero: TUVec4;
begin
  Result := Make(0);
end;

class function TUVec4Impl.Make(const Ax, Ay, Az, Aw: TUFloat): TUVec4;
begin
  Result[0] := Ax;
  Result[1] := Ay;
  Result[2] := Az;
  Result[3] := Aw;
end;

class function TUVec4Impl.Make(const v: TUVec3; const Aw: TUFloat): TUVec4;
begin
  Result := Make(v.x, v.y, v.z, Aw);
end;

class function TUVec4Impl.Make(const v0, v1: TUVec2): TUVec4;
begin
  Result := Make(v0.x, v0.y, v1.x, v1.y);
end;

class function TUVec4Impl.Make(const v: TUVec2; const Az, Aw: TUFloat): TUVec4;
begin
  Result := Make(v.x, v.y, Az, Aw);
end;

class function TUVec4Impl.Make(const s: TUFloat): TUVec4;
begin
  Result := Make(s, s, s, s);
end;

function TUMatImpl.GetElement(const Index: UInt32): TUFloat;
begin
  Result := Self[Index shr 2, Index mod 4];
end;

procedure TUMatImpl.SetElement(const Index: UInt32; const Value: TUFloat);
begin
  Self[Index shr 2, Index mod 4] := Value;
end;

class function TUMatImpl.Make(
  const e00, e10, e20, e30: TUFloat;
  const e01, e11, e21, e31: TUFloat;
  const e02, e12, e22, e32: TUFloat;
  const e03, e13, e23, e33: TUFloat
): TUMat;
begin
  Result[0, 0] := e00; Result[1, 0] := e10; Result[2, 0] := e20; Result[3, 0] := e30;
  Result[0, 1] := e01; Result[1, 1] := e11; Result[2, 1] := e21; Result[3, 1] := e31;
  Result[0, 2] := e02; Result[1, 2] := e12; Result[2, 2] := e22; Result[3, 2] := e32;
  Result[0, 3] := e03; Result[1, 3] := e13; Result[2, 3] := e23; Result[3, 3] := e33;
end;


class function TUMatImpl.Zero: TUMat;
begin
  Result := Make(
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  );
end;

class function TUMatImpl.Identity: TUMat;
begin
  Result := Make(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
end;

function TUColorImpl.GetChannel(const Index: UInt8): UInt8;
  var Arr: array[0..3] of UInt8 absolute Self;
begin
  Result := Arr[Index];
end;

procedure TUColorImpl.SetChannel(const Index: UInt8; const Value: UInt8);
  var Arr: array[0..3] of UInt8 absolute Self;
begin
  Arr[Index] := Value;
end;

function TUColorImpl.GetR: UInt8;
begin
  Result := Self.Channel[0];
end;

procedure TUColorImpl.SetR(const Value: UInt8);
begin
  Channel[0] := Value;
end;

function TUColorImpl.GetG: UInt8;
begin
  Result := Channel[1];
end;

procedure TUColorImpl.SetG(const Value: UInt8);
begin
  Channel[1] := Value;
end;

function TUColorImpl.GetB: UInt8;
begin
  Result := Channel[2];
end;

procedure TUColorImpl.SetB(const Value: UInt8);
begin
  Channel[2] := Value;
end;

function TUColorImpl.GetA: UInt8;
begin
  Result := Channel[3];
end;

procedure TUColorImpl.SetA(const Value: UInt8);
begin
  Channel[3] := Value;
end;

class function TUColorImpl.Make(const Ar, Ag, Ab: UInt8; const Aa: UInt8): TUColor;
begin
  Result := Ar or (Ag shl 8) or (Ab shl 16) or (Aa shl 24);
end;

function TUColorImpl.Inverse: TUColor;
begin
  Result := TUColor.Make(255 - r, 255 - g, 255 - b, 255 - a);
end;

function TUColorImpl.AsVec4: TUVec4;
begin
  Result := TUVec4.Make(r * URcp255, g * URcp255, b * URcp255, a * URcp255);
end;

function TUColorImpl.AsVec3: TUVec3;
begin
  Result := TUVec3.Make(r * URcp255, g * URcp255, b * URcp255);
end;

constructor TStreamHelper.Create(const Buffer: Pointer; const BufferSize: UInt32);
begin
  _Ptr := Buffer;
  _Pos := 0;
  _Size := BufferSize;
end;

destructor TStreamHelper.Destroy;
begin
  inherited Destroy;
end;

generic function TStreamHelper.Read<T>: T;
  type PT = ^T;
begin
  Result := PT(_Ptr + _Pos)^;
  Inc(_Pos, SizeOf(T));
end;

procedure TStreamHelper.ReadBuffer(const Buffer: Pointer; const BufferSize: UInt32);
begin
  Move((_Ptr + _Pos)^, Buffer^, BufferSize);
  Inc(_Pos, BufferSize);
end;

function TStreamHelper.ReadUInt8: UInt8;
begin
  Result := specialize Read<UInt8>;
end;

procedure TStreamHelper.Skip(const Size: UInt32);
begin
  _Pos += Size;
end;

procedure TFont.Initialize(const FontFace: String; const Bold: Boolean);
  type TARGB = packed record
    b, g, r, a: UInt8;
  end;
  type TARGBArr = array[Word] of TARGB;
  type PARGBArr = ^TARGBArr;
  var dc: THandle;
  var Font: THandle;
  var Bitmap: THandle;
  var bmi: TBitmapInfo;
  var BitmapBits: Pointer;
  var i, x, y: Int32;
  var MapWidth, MapHeight: Int32;
  var MaxWidth, MaxHeight: Int32;
  var CharSize: TSize;
  var TextureData: Pointer;
  var FontStyle: LongInt;
begin
  Size := 32;
  //Face := 'Times New Roman';
  Face := FontFace;
  if Bold then FontStyle := FW_BOLD
  else FontStyle := FW_NORMAL;
  //Face := 'Courier New';
  MaxWidth := 2048; MaxHeight := 2048;
  dc := CreateCompatibleDC(0);
  SetMapMode(dc, MM_TEXT);
  Font := CreateFontA(
    Size, 0, 0, 0,
    //FW_NORMAL,
    FontStyle,
    0, 0, 0,
    DEFAULT_CHARSET,
    OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS,
    ANTIALIASED_QUALITY,
    VARIABLE_PITCH,
    PAnsiChar(Face)
  );
  SelectObject(dc, Font);
  CharSpaceX := 0;
  CharSpaceY := 0;
  CharSize := Default(TSize);
  for i := 0 to 255 do
  begin
    GetTextExtentPoint32A(dc, PAnsiChar(@i), 1, CharSize);
    if CharSize.cx > CharSpaceX then CharSpaceX := CharSize.cx;
    if CharSize.cy > CharSpaceY then CharSpaceY := CharSize.cy;
  end;
  MapWidth := Int32(CharSpaceX * 16);
  MapHeight := Int32(CharSpaceY * 16);
  TexWidth := 1; while TexWidth < MapWidth do TexWidth := TexWidth shl 1; if TexWidth > MaxWidth then TexWidth := MaxWidth;
  TexHeight := 1; while TexHeight < MapHeight do TexHeight := TexHeight shl 1; if TexHeight > MaxHeight then TexHeight := MaxHeight;
  CharSpaceX := TexWidth div 16;
  CharSpaceY := TexHeight div 16;
  bmi := Default(TBitmapInfo);
  FillChar(bmi.bmiHeader, SizeOf(TBitmapInfoHeader), 0);
  bmi.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  bmi.bmiHeader.biWidth :=  TexWidth;
  bmi.bmiHeader.biHeight := -TexHeight;
  bmi.bmiHeader.biPlanes := 1;
  bmi.bmiHeader.biCompression := BI_RGB;
  bmi.bmiHeader.biBitCount := 32;
  BitmapBits := nil;
  Bitmap := CreateDIBSection(
    dc,
    bmi,
    DIB_RGB_COLORS,
    Pointer(BitmapBits),
    0, 0
  );
  SelectObject(dc, Bitmap);
  SetTextColor(dc, $ffffff);
  SetBkColor(dc, $00000000);
  SetTextAlign(dc, TA_TOP);
  for y := 0 to 15 do
  for x := 0 to 15 do
  begin
    i := x + y * 16;
    GetTextExtentPoint32A(dc, PAnsiChar(@i), 1, CharSize);
    CharProps[i].Width := CharSize.cx;
    CharProps[i].Height := CharSize.cy;
    CharProps[i].OffsetX := (CharSpaceX - CharProps[i].Width) div 2;
    CharProps[i].OffsetY := (CharSpaceY - CharProps[i].Height) div 2;
    ExtTextOut(
      dc,
      x * CharSpaceX + CharProps[i].OffsetX,
      y * CharSpaceY + CharProps[i].OffsetY,
      ETO_OPAQUE,
      nil,
      PAnsiChar(@i),
      1,
      nil
    );
  end;
  GetMem(TextureData, TexWidth * TexHeight * 4);
  {$push}
  {$R-}
  for y := 0 to TexWidth - 1 do
  for x := 0 to TexHeight - 1 do
  begin
    i := y * TexWidth + x;
    PARGBArr(TextureData)^[i].a := PARGBArr(BitmapBits)^[i].r;
    PARGBArr(TextureData)^[i].r := $ff;
    PARGBArr(TextureData)^[i].g := $ff;
    PARGBArr(TextureData)^[i].b := $ff;
  end;
  {$pop}
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glGenTextures(1, @TextureId);
  glBindTexture(GL_TEXTURE_2D, TextureId);
  glTexImage2D(
    GL_TEXTURE_2D,
    0,
    GL_RGBA,
    TexWidth,
    TexHeight,
    0,
    GL_RGBA,
    GL_UNSIGNED_BYTE,
    TextureData
  );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindTexture(GL_TEXTURE_2D, 0);
  CheckGLErrors;
  FreeMem(TextureData, TexWidth * TexHeight * 4);
  DeleteObject(Bitmap);
  DeleteObject(Font);
  DeleteDC(dc);
end;

procedure TFont.Finalize;
begin
  glDeleteTextures(1, @TextureId);
end;

procedure TFont.Print(const Pos: TUVec2; const Scale: TUVec2; const Text: String; const Color: TUColor);
  var i: Int32;
  var c: UInt8;
  var tu1, tv1, tu2, tv2: Single;
  var x1, y1, x2, y2: Single;
  var CharTU, CharTV, CurPos: Single;
  var ColV: TUVec4;
begin
  CharTU := CharSpaceX / TexWidth;
  CharTV := CharSpaceY / TexHeight;
  CurPos := Pos.x;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, TextureId);
  glBegin(GL_QUADS);
  ColV := Color.AsVec4;
  glColor4fv(@ColV);
  for i := 0 to Length(Text) - 1 do
  begin
    c := Ord(Text[i + 1]);
    tu1 := (c mod 16) * CharTU;
    tv1 := (c div 16) * CharTV;
    tu2 := tu1 + CharTU;
    tv2 := tv1 + CharTV;
    x1 := CurPos - CharProps[c].OffsetX * Scale.x;
    y1 := Pos.y - CharProps[c].OffsetY * Scale.y;
    x2 := x1 + CharSpaceX * Scale.x;
    y2 := y1 - CharSpaceY * Scale.x;
    CurPos := CurPos + CharProps[c].Width * Scale.x;
    glTexCoord2f(tu1, tv1); glVertex2f(x1, y1);
    glTexCoord2f(tu2, tv1); glVertex2f(x2, y1);
    glTexCoord2f(tu2, tv2); glVertex2f(x2, y2);
    glTexCoord2f(tu1, tv2); glVertex2f(x1, y2);
  end;
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
end;

function TFont.TextWidth(const Text: String): Int32;
  var i: Int32;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  Result := Result + CharProps[Ord(Text[i])].Width;
end;

function TFont.TextHeight(const Text: String): Int32;
  var i: Int32;
  var b: Uint8;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  begin
    b := Ord(Text[i]);
    if CharProps[b].Height > Result then
    Result := CharProps[b].Height;
  end;
end;

function UClamp(const v, MinV, MaxV: Int8): Int8;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: Int16): Int16;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: Int32): Int32;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: Int64): Int64;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: UInt8): UInt8;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: UInt16): UInt16;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: UInt32): UInt32;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: UInt64): UInt64;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: TUFloat): TUFloat;
begin
  if v < MinV then Exit(MinV) else if v > MaxV then Exit(MaxV) else Exit(v);
end;

function UClamp(const v, MinV, MaxV: TUVec2): TUVec2;
  var i: Int32;
begin
  for i := 0 to High(TUVec2) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

function UClamp(const v, MinV, MaxV: TUVec3): TUVec3;
  var i: Int32;
begin
  for i := 0 to High(TUVec3) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

function UClamp(const v, MinV, MaxV: TUVec4): TUVec4;
  var i: Int32;
begin
  for i := 0 to High(TUVec4) do Result[i] := UClamp(v[i], MinV[i], MaxV[i]);
end;

generic function UEndianSwap<T>(const v: T): T;
  type TByteArr = array[0..SizeOf(T) - 1] of UInt8;
  var Src: TByteArr absolute v;
  var Dst: TByteArr absolute Result;
  var i: Int32;
begin
  for i := 0 to High(TByteArr) do
  Dst[i] := Src[High(TByteArr) - i];
end;

function UEndianSwap(const v: UInt16): UInt16;
begin
  Result := specialize UEndianSwap<UInt16>(v);
end;

function UEndianSwap(const v: UInt32): UInt32;
begin
  Result := specialize UEndianSwap<UInt32>(v);
end;

procedure USinCos(const a: TUFloat; out s: TUFloat; out c: TUFloat);
begin
  s := Sin(a);
  c := Cos(a);
end;

function UIntToStr(const v: Int32): String;
begin
  System.Str(v, Result);
end;

generic procedure UArrSort<T>(var Arr: array of T);
  procedure SortRange(const RangeStart, RangeEnd: Integer); overload;
    var i, j: Integer;
    var tmp, pivot: T;
  begin
    if RangeEnd <= RangeStart then exit;
    i := RangeStart;
    j := RangeEnd;
    pivot := Arr[(RangeStart + RangeEnd) shr 1];
    repeat
      while (pivot > Arr[i]) do i := i + 1;
      while (Arr[j] > pivot) do j := j - 1;
      if i <= j then
      begin
        tmp := Arr[i];
        Arr[i] := Arr[j];
        Arr[j] := tmp;
        j := j - 1;
        i := i + 1;
      end;
    until i > j;
    if RangeStart < j then SortRange(RangeStart, j);
    if i < RangeEnd then SortRange(i, RangeEnd);
  end;
begin
  SortRange(Low(Arr), High(Arr));
end;

generic procedure UArrAppend<T>(var Arr: specialize TUArray<T>; const Item: T);
begin
  SetLength(Arr, Length(Arr) + 1);
  Arr[High(Arr)] := Item;
end;

operator + (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
end;

operator - (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
end;

operator * (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
end;

operator / (const v0, v1: TUVec2): TUVec2;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
end;

operator * (const v: TUVec2; const f: TUFloat): TUVec2;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
end;

operator * (const f: TUFloat; const v: TUVec2): TUVec2;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
end;

operator - (const v: TUVec2): TUVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

operator + (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
  Result[2] := v0[2] + v1[2];
end;

operator - (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
  Result[2] := v0[2] - v1[2];
end;

operator * (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
  Result[2] := v0[2] * v1[2];
end;

operator / (const v0, v1: TUVec3): TUVec3;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
  Result[2] := v0[2] / v1[2];
end;

operator * (const v: TUVec3; const f: TUFloat): TUVec3;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
end;

operator * (const f: TUFloat; const v: TUVec3): TUVec3;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
end;

operator - (const v: TUVec3): TUVec3;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
end;

operator + (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] + v1[0];
  Result[1] := v0[1] + v1[1];
  Result[2] := v0[2] + v1[2];
  Result[3] := v0[3] + v1[3];
end;

operator - (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] - v1[0];
  Result[1] := v0[1] - v1[1];
  Result[2] := v0[2] - v1[2];
  Result[3] := v0[3] - v1[3];
end;

operator * (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] * v1[0];
  Result[1] := v0[1] * v1[1];
  Result[2] := v0[2] * v1[2];
  Result[3] := v0[3] * v1[3];
end;

operator / (const v0, v1: TUVec4): TUVec4;
begin
  Result[0] := v0[0] / v1[0];
  Result[1] := v0[1] / v1[1];
  Result[2] := v0[2] / v1[2];
  Result[3] := v0[3] / v1[3];
end;

operator * (const v: TUVec4; const f: TUFloat): TUVec4;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
  Result[3] := v[3] * f;
end;

operator * (const f: TUFloat; const v: TUVec4): TUVec4;
begin
  Result[0] := v[0] * f;
  Result[1] := v[1] * f;
  Result[2] := v[2] * f;
  Result[3] := v[3] * f;
end;

operator - (const v: TUVec4): TUVec4;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
  Result[3] := -v[3];
end;

operator mod (const a, b: TUFloat): TUFloat;
begin
  Result := a - b * Int(a / b);
end;

end.

