unit GameUnit;

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
  Utils,
  Audio,
  Data;

type TBarrier = record
public
  var p: TUVec2;
  var s: Single;
  var w: Single;
  var mp: Single;
  var Closed: Boolean;
  var Moving: Boolean;
  procedure Initialize;
  procedure Finalize;
  procedure Update(const dt: Single);
  procedure Render;
  procedure Reset;
  function Rect(const Up: Boolean): TUVec4;
end;

type TBird = record
public
  var p: TUVec2;
  var v: TUVec2;
  var s: Single;
  var f: Single;
  procedure Initialize;
  procedure Finalize;
  procedure Update(const dt: Single);
  procedure Render;
end;

type TCrash = record
public
  var Crashed: Boolean;
  var p: TUVec2;
  procedure Initialize;
  procedure Render;
end;

type TTimer = record
private
  var _Interval: Double;
  var _Average: Double;
  var tb: Double;
  var t: Uint64;
  var tc: Uint64;
  procedure Initialize;
  procedure Finalize;
public
  property Average: Double read _Average;
  procedure Reset(const Interval: Double = 1000.0);
  procedure Start;
  function Stop: Double;
  class operator Initialize(var Obj: TTimer);
  class operator Finalize(var Obj: TTimer);
end;

type TWindow = class
private
  class var _WndClassName: String;
  var _Handle: THandle;
  var _DeviceContext: THandle;
  var _RenderingContext: THandle;
  var _Caption: String;
  var _KeyPressed: Boolean;
  function GetWindowStyle: UInt32;
  function GetWindowSize: TPoint;
  function GetScreenSize: TPoint;
  procedure InitializeGL;
  procedure FinalizeGL;
  procedure SetCaption(const Value: String);
public
  property Handle: THandle read _Handle;
  property Caption: String read _Caption write SetCaption;
  property Size: TPoint read GetWindowSize;
  property KeyPressed: Boolean read _KeyPressed write _KeyPressed;
  constructor Create;
  destructor Destroy; override;
  procedure DrawBegin;
  procedure DrawEnd;
  procedure KeyUp;
  procedure KeyDown;
  procedure MouseDown;
  procedure MouseUp;
  procedure Close;
  class constructor CreateClass;
  class destructor DestroyClass;
end;

type TGame = class
public
  var Window: TWindow;
  var Audio: TAudio;
  //var Music: TAudio.TStream;
  //var Sound: TAudio.TSynth;
  var Running: Boolean;
  var Bird: TBird;
  var Crash: TCrash;
  var ResetDelay: Single;
  var Playing: Boolean;
  var Barriers: array [0..5] of TBarrier;
  var FontN: TFont;
  var FontB: TFont;
  var Score: Int32;
  var HighScore: Int32;
  var Tempo: Single;
  const PlayArea = 42;
  constructor Create;
  destructor Destroy; override;
  procedure Initialize;
  procedure Finalize;
  procedure Loop;
  procedure Update(const dt: Single);
  procedure Render;
  procedure Reset;
  procedure PrintScore;
  procedure PrintStart;
  function WasKeyPressed(): Boolean;
  function MakeRotation(const a: Single): TUVec2;
  function RotateVec2(const v, r: TUVec2): TUVec2;
  function ProjToRect(const v: TUVec2; const r: TUVec4): TUVec2;
  procedure DrawRect(const TopLeft, BottomRight: TUVec2; const Color: TUColor = $ffffffff);
  procedure DrawBorder(const r: TUVec4; const s: Single; const c0, c1: TUColor);
  procedure DrawQuad(const v0, v1, v2, v3: TUVec2; const Color: TUColor = $ffffffff);
  procedure DrawQuadCol(const v0, v1, v2, v3: TUVec2; const c0, c1, c2, c3: TUColor);
  procedure DrawCircle(const c: TUVec2; const r: Single; const Color: TUColor = $ffffffff);
  procedure DrawLine(const p0, p1: TUVec2; const s: Single = 0.1; const Color: TUColor = $ffffffff);
  procedure PrintOutlined(
    const p: TUVec2; const s: TUVec2; const Text: String; const Color: TUColor = $ffffffff;
    const Bold: Boolean = False; const OutlineScale: Single = 1
  );
  class procedure Main;
end;

var Game: TGame;

implementation

procedure TBarrier.Initialize;
begin
  p := TUVec2.Make(0);
  s := 5;
  w := 1;
end;

procedure TBarrier.Finalize;
begin

end;

procedure TBarrier.Update(const dt: Single);
  var pp: TUVec2;
  var cp: Single;
begin
  if (not Game.Playing) then Exit;
  pp := p;
  p.x := p.x - dt * 7;
  cp := Game.Bird.p.x - Game.Bird.s - w - 0.1;
  if (cp <= pp.x) and (cp > p.x) then Game.Score += 1;
  if p.x < -(Game.PlayArea div 2) then Reset;
end;

procedure TBarrier.Render;
  procedure DrawRect(const r: TUVec4);
    const b = 0.1;
    const bb = b * 1;
    var c0, c1: TUColor;
  begin
    c0 := TUColor.Make(106, 252, 118);
    c1 := TUColor.Make(47, 212, 61);
    Game.DrawRect(
      TUVec2.Make(r.x - b, r.y - b),
      TUVec2.Make(r.z + b, r.w + b),
      $ff000000
    );
    Game.DrawRect(
      TUVec2.Make(r.x, r.y),
      TUVec2.Make((r.x + r.z) * 0.5, r.w),
      c0
    );
    Game.DrawRect(
      TUVec2.Make((r.x + r.z) * 0.5, r.y),
      TUVec2.Make(r.z, r.w),
      c1
    );
    Game.DrawBorder(
      TUVec4.Make(r.x - b, r.y - b, r.z + b, r.w + b),
      bb, $ff000000, $00000000
    );
  end;
  var r: TUVec4;
  var b: Boolean;
begin
  for b := False to True do
  begin
    r := Rect(b);
    DrawRect(r);
    r.x := r.x - w * 0.2;
    r.z := r.z + w * 0.2;
    if b then
    begin
      r.w := r.y + w;
    end
    else
    begin
      r.y := r.w - w;
    end;
    DrawRect(r);
  end;
end;

procedure TBarrier.Reset;
begin
  if (p.x < -(Game.PlayArea div 2)) then p.x := p.x + Game.PlayArea;
  p.y := (Random * 2  - 1) * 4;
  mp := (Random * 2  - 1) * 4;
  s := 6.5 + Random * 2.5;
  Closed := (Game.Score > 10) and (Random(5) = 0);
  Moving := (Game.Score > 20) and (Random(3) = 0);
end;

function TBarrier.Rect(const Up: Boolean): TUVec4;
  var ss: Single;
  var y: Single;
begin
  if Moving then
  begin
    y := ULerp(mp, p.y, USmoothStep(p.x - Game.Bird.p.x, 6, 8));
  end
  else
  begin
    y := p.y;
  end;
  if Closed then
  begin
    ss := 1 - (UClamp(Abs(p.x - Game.Bird.p.x), 3, 6) - 3) / 3;
  end
  else
  begin
    ss := 1;
  end;
  if (Up) then
  begin
    Result := TUVec4.Make(
      p.x - w, y + s * 0.5 * ss,
      p.x + w, y + 20
    );
  end
  else
  begin
    Result := TUVec4.Make(
      p.x - w, y - 20,
      p.x + w, y - s * 0.5 * ss
    );
  end;
end;

procedure TBird.Initialize;
begin
  s := 0.5;
  p := TUVec2.Make(-9, 0);
  v := TUVec2.Make(0);
  f := 0;
end;

procedure TBird.Finalize;
begin

end;

procedure TBird.Update(const dt: Single);
  var i: Int32;
  var b: Boolean;
  var r: TUVec4;
  var d, c: TUVec2;
  var sp: Uint8;
begin
  if (not Game.Playing) then Exit;
  if (f > 0) then f -= dt;
  v := v + TUVec2.Make(0, -100 * dt);
  if Game.WasKeyPressed() and not Game.Crash.Crashed then
  begin
    v := TUVec2.Make(0, 25);
    f := 0.1;
    sp := 60 + Round(p.y + 10);
    if sp < 60 then sp := 60 else if sp > 80 then sp := 80;
    Game.Audio.PlaySound($78, sp, 100);
  end;
  p := p + v * dt;
  if p.y < -11 then p.y := -11;
  if p.y > 11 then p.y := 11;
  if not Game.Crash.Crashed then
  begin
    for i := 0 to High(Game.Barriers) do
    begin
      for b := False to True do
      begin
        r := Game.Barriers[i].Rect(b);
        c := Game.ProjToRect(p, r);
        d := c - p;
        if (d.Len < s) then
        begin
          Game.Crash.Crashed := True;
          Game.Crash.p := c;
          Game.Audio.PlaySound($7f, $38 + Random(10) - 5, 85);
          Break;
        end;
      end;
      if (Game.Crash.Crashed) then Break;
    end;
  end;
end;

procedure TBird.Render;
  var dir, n, w: TUVec2;
  var col: TUColor;
  const b = 0.15;
begin
  col := TUColor.Make(250, 201, 40);
  dir := TUVec2.Make(15, v.y).Norm;
  n := TUVec2.Make(dir.y, -dir.x);
  Game.DrawCircle(p, 1 + b, $ff000000);
  Game.DrawCircle(p, 1, col);
  Game.DrawQuad(
    p + n * (0.4 + b) + dir * (0.9 + b),
    p + n * (0.1 + b) + dir * (1.5 + b),
    p - n * (0.1 + b) + dir * (1.5 + b),
    p - n * (0.4 + b) + dir * (0.9 + b),
    $ff000000
  );
  Game.DrawQuad(
    p + n * 0.4 + dir * 0.9,
    p + n * 0.1 + dir * 1.5,
    p - n * 0.1 + dir * 1.5,
    p - n * 0.4 + dir * 0.9,
    TUColor.Make(232, 55, 39)
  );
  Game.DrawQuad(
    p + n * (0.4 + b) - dir * (1.6 + b),
    p + n * (0.2 + b) - dir * (0.9 + b),
    p - n * (0.2 + b) - dir * (0.9 + b),
    p - n * (0.4 + b) - dir * (1.6 + b),
    $ff000000
  );
  Game.DrawQuad(
    p + n * 0.4 - dir * 1.6,
    p + n * 0.2 - dir * 0.9,
    p - n * 0.2 - dir * 0.9,
    p - n * 0.4 - dir * 1.6,
    TUColor.Make(48, 217, 98)
  );
  if (Game.Crash.Crashed) then
  begin
    Game.DrawLine(
      p - n * 0.4 + dir * 0.4 + TUVec2.Make(-0.3, -0.3),
      p - n * 0.4 + dir * 0.4 + TUVec2.Make(0.3, 0.3),
      0.2, $ff000000
    );
    Game.DrawLine(
      p - n * 0.4 + dir * 0.4 + TUVec2.Make(0.3, -0.3),
      p - n * 0.4 + dir * 0.4 + TUVec2.Make(-0.3, 0.3),
      0.2, $ff000000
    );
  end
  else
  begin
    Game.DrawCircle(p - n * 0.4 + dir * 0.4, 0.5 + b, $ff000000);
    Game.DrawCircle(p - n * 0.4 + dir * 0.4, 0.5);
    Game.DrawCircle(p - n * 0.4 + dir * 0.7, 0.3, $ff000000);
  end;
  w := (dir * -0.6) + (n * -0.2);
  if (f > 0) or (not Game.Playing and (GetTickCount64 mod 400 > 200)) then
  begin
    Game.DrawQuad(
      p + w + n * 1.2 - dir * 0.7,
      p + w + n * 0.8 + dir * 0.5,
      p + w - n * 0 + dir * 0.3,
      p + w - n * 0 - dir * 0.3,
      TUColor.Make(45, 109, 227)
    );
  end
  else
  begin
    Game.DrawQuad(
      p + w + n * 0 - dir * 0.3,
      p + w + n * 0 + dir * 0.3,
      p + w - n * 0.8 + dir * 0.5,
      p + w - n * 1.2 - dir * 0.7,
      TUColor.Make(45, 109, 227)
    );
  end;
end;

procedure TCrash.Initialize;
begin
  p := TUVec2.Zero;
  Crashed := False;
end;

procedure TCrash.Render;
  var rs: UInt32;
  function Rnd: Single;
    var cf: Single;
  begin
    {$push}
    {$R-}
    cf := 1 / 32768;
    rs := rs * 1103515245 + 12345;
    Result := Single((rs div 65536) mod 32768) * cf;
    {$pop}
  end;
  var i: Int32;
  var v0, v1, v2: TUVec2;
  var r: TUVec2;
  var c0, c1, c2: TUColor;
  const seg = 12;
begin
  if (not Crashed) then Exit;
  r := Game.MakeRotation(pi * 2 / (seg * 2));
  v0 := TUVec2.Make(1, 0);
  v1 := Game.RotateVec2(v0, r);
  v2 := Game.RotateVec2(v1, r);
  c0 := TUColor.Make(255, 255, 255);
  c1 := TUColor.Make(252, 246, 121);
  c2 := TUColor.Make(250, 154, 45);
  for i := 0 to seg - 1 do
  begin
    Game.DrawQuadCol(
      p, p + v0 * 0.6, p + v1 * (1.5 + Rnd * 1.5), p + v2 * 0.6,
      c0, c1, c2, c1
    );
    v0 := v2;
    v1 := Game.RotateVec2(v0, r);
    v2 := Game.RotateVec2(v1, r);
  end;
end;

procedure TTimer.Initialize;
begin
  Reset();
end;

procedure TTimer.Finalize;
begin

end;

procedure TTimer.Reset(const Interval: Double);
begin
  _Interval := Interval;
  t := 0;
  tb := 0;
  tc := 0;
  _Average := 0;
end;

procedure TTimer.Start;
begin
  t := GetTickCount64;
end;

function TTimer.Stop: Double;
  var dt: Uint64;
begin
  dt := GetTickCount64 - t;
  Result := Double(dt) * 0.001;
  tb += dt;
  tc += 1;
  if tb >= _Interval then
  begin
    _Average := (Double(tb) / Double(tc)) * 0.001;
    tb := 0;
    tc := 0;
  end;
end;

class operator TTimer.Initialize(var Obj: TTimer);
begin
  Obj.Initialize;
end;

class operator TTimer.Finalize(var Obj: TTimer);
begin
  Obj.Finalize;
end;

function TWindow.GetWindowStyle: UInt32;
begin
  Result := (
    WS_POPUP or
    WS_VISIBLE or
    WS_EX_TOPMOST
  );
  if (True){add caption} then
  begin
    Result := Result or (
      WS_CAPTION or
      WS_MINIMIZEBOX or
      WS_MAXIMIZEBOX or
      WS_SYSMENU
    );
    if (True){make resizable} then
    begin
      Result := Result or WS_THICKFRAME;
    end;
  end;
end;

function TWindow.GetWindowSize: TPoint;
  var R: TRect;
begin
  if (_Handle <> 0) then
  begin
    R := Default(TRect);
    GetClientRect(_Handle, R);
    Result.X := R.Right - R.Left;
    Result.Y := R.Bottom - R.Top;
  end
  else
  begin
    Result.X := 900;
    Result.Y := 520;
  end;
end;

function TWindow.GetScreenSize: TPoint;
begin
  Result.X := GetSystemMetrics(SM_CXSCREEN);
  Result.Y := GetSystemMetrics(SM_CYSCREEN);
end;

procedure TWindow.InitializeGL;
  var pfd: TPixelFormatDescriptor;
  var pf: Int32;
begin
  _DeviceContext := GetDC(_Handle);
  pfd := Default(TPixelFormatDescriptor);
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;
  pfd.cAlphaBits := 8;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(_DeviceContext, @pfd);
  SetPixelFormat(_DeviceContext, pf, @pfd);
  _RenderingContext := wglCreateContext(_DeviceContext);
  wglMakeCurrent(_DeviceContext, _RenderingContext);
end;

procedure TWindow.FinalizeGL;
begin
  wglMakeCurrent(_DeviceContext, _RenderingContext);
  wglDeleteContext(_RenderingContext);
  ReleaseDC(_Handle, _DeviceContext);
end;

procedure TWindow.SetCaption(const Value: String);
begin
  if _Caption = Value then Exit;
  _Caption := Value;
  SetWindowTextA(_Handle, PChar(_Caption));
end;

constructor TWindow.Create;
  var Style: UInt32;
  var WindowSize, ScreenSize, Position: TPoint;
begin
  _Caption := 'TwittyFlapper';
  Style := GetWindowStyle;
  WindowSize := GetWindowSize;
  ScreenSize := GetScreenSize;
  Position.X := (ScreenSize.x - Size.x) shr 1;
  Position.Y := (ScreenSize.y - Size.y) shr 1;
  _Handle := CreateWindowExA(
    0, PAnsiChar(_WndClassName), PAnsiChar(_Caption),
    Style, Position.x, Position.y, WindowSize.x, WindowSize.y,
    0, 0, HInstance, nil
  );
  SetWindowLong(_handle, GWL_USERDATA, UInt32(PtrUInt(self)));
  ShowWindow(_Handle, SW_NORMAL);
  BringWindowToTop(_Handle);
  InitializeGL;
end;

destructor TWindow.Destroy;
begin
  FinalizeGL;
  DestroyWindow(_Handle);
  _Handle := 0;
  inherited Destroy;
end;

procedure TWindow.DrawBegin;
  var WinRect: TRect;
begin
  WinRect := Default(TRect);
  GetClientRect(_Handle, WinRect);
  glViewport(0, 0, WinRect.Right - WinRect.Left, WinRect.Bottom - WinRect.Top);
end;

procedure TWindow.DrawEnd;
begin
  SwapBuffers(_DeviceContext);
end;

procedure TWindow.KeyUp;
begin

end;

procedure TWindow.KeyDown;
begin
  _KeyPressed := True;
end;

procedure TWindow.MouseDown;
begin
  _KeyPressed := True;
end;

procedure TWindow.MouseUp;
begin

end;

procedure TWindow.Close;
begin
  Game.Running := False;
end;

function WindowProc(Wnd: THandle; Msg: UInt32; WParam: PtrInt; LParam: PtrInt): PtrInt; stdcall;
  var Window: TWindow;
begin
  Window := TWindow(PtrUInt(GetWindowLong(Wnd, GWL_USERDATA)));
  if Assigned(Window) then
  case msg of
    WM_DESTROY:
    begin
      Result := 0;
      Exit;
    end;
    WM_CLOSE:
    begin
      Window.Close;
      Result := 0;
      Exit;
    end;
    WM_CHAR:
    begin
    end;
    WM_KEYDOWN:
    begin
      Window.KeyDown;
    end;
    WM_KEYUP:
    begin
      Window.KeyUp;
    end;
    WM_LBUTTONDOWN:
    begin
      Window.MouseDown;
    end;
    WM_LBUTTONDBLCLK:
    begin
    end;
    WM_LBUTTONUP:
    begin
      Window.MouseUp;
    end;
    WM_RBUTTONDOWN:
    begin
    end;
    WM_RBUTTONDBLCLK:
    begin
    end;
    WM_RBUTTONUP:
    begin
    end;
    WM_MBUTTONDOWN:
    begin
    end;
    WM_MBUTTONDBLCLK:
    begin
    end;
    WM_MBUTTONUP:
    begin
    end;
    WM_MOUSEWHEEL:
    begin
    end;
    WM_SETCURSOR:
    begin
    end;
    WM_SIZE:
    begin
    end;
  end;
  Result := DefWindowProc(Wnd, Msg, WParam, LParam);
end;

class constructor TWindow.CreateClass;
  var WndClass: TWndClassExA;
begin
  _WndClassName := 'GameWindow';
  WndClass := Default(TWndClassExA);
  FillChar(WndClass, SizeOf(WndClass), 0);
  WndClass.cbSize := SizeOf(TWndClassExA);
  WndClass.hIconSm := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hIcon := LoadIcon(MainInstance, 'MAINICON');
  WndClass.hInstance := HInstance;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  WndClass.lpszClassName := PAnsiChar(_WndClassName);
  WndClass.style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC or CS_DBLCLKS;
  WndClass.lpfnWndProc := @WindowProc;
  if RegisterClassExA(WndClass) = 0 then
  begin
    _WndClassName := 'static';
  end;
end;

class destructor TWindow.DestroyClass;
begin
  if (_WndClassName <> 'static') then
  begin
    UnregisterClassA(PAnsiChar(_WndClassName), HInstance);
  end;
end;

constructor TGame.Create;
  var Midi: TMidiFile;
begin
  inherited Create;
  Window := TWindow.Create;
  FontN.Initialize('Arial', False);
  FontB.Initialize('Arial', True);
  Midi.Load(@bin_Music2_mid, SizeOf(bin_Music2_mid));
  Audio.Initialize;
  Audio.PlayMidi(Midi);
end;

destructor TGame.Destroy;
begin
  Audio.Finalize;
  FontN.Finalize;
  FontB.Finalize;
  Window.Free;
  inherited Destroy;
end;

procedure TGame.Initialize;
begin
  HighScore := 0;
  Tempo := 0;
  Running := True;
  Randomize;
  Reset;
end;

procedure TGame.Finalize;
  var i: Int32;
begin
  for i := 0 to High(Barriers) do
  begin
    Barriers[i].Finalize;
  end;
  Bird.Finalize;
end;

procedure TGame.Loop;
  var m: TMsg;
  var Timer: TTimer;
  var dt: Single;
begin
  m := Default(TMsg);
  dt := 1 / 30;
  while Running do
  begin
    Timer.Start;
    FillChar(m, SizeOf(m), 0);
    Window.KeyPressed := False;
    while PeekMessage(m, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage(m);
      DispatchMessage(m);
      if (m.hwnd = 0) and (m.message = WM_QUIT) then
      begin
        Running := False;
      end;
    end;
    Update(dt);
    Window.DrawBegin;
    Render;
    Window.DrawEnd;
    if (Timer.Average > 0) then
    begin
      Window.Caption := 'TwittyFlapper FPS: ' + UIntToStr(Round(1 / Timer.Average));
    end;
    Sleep(1);
    dt := Timer.Stop;
  end;
end;

procedure TGame.Update(const dt: Single);
  var i: Int32;
begin
  if Playing and (Tempo < 1) then Tempo := UClamp(Tempo + dt * 0.5, 0, 1);
  if not Playing and (Tempo > 0) then Tempo := UClamp(Tempo - dt * 0.5, 0, 1);
  Audio.Update(Round(dt * (100 + 100 * Tempo + UClamp(Score, 0, 100))));
  if Crash.Crashed then
  begin
    if ResetDelay > 0 then ResetDelay -= dt;
    if ResetDelay <= 0 then
    begin
      Reset;
    end;
  end;
  if (not Playing and WasKeyPressed) then
  begin
    Playing := True;
  end;
  if (not Crash.Crashed) then
  for i := 0 to High(Barriers) do
  begin
    Barriers[i].Update(dt);
  end;
  Bird.Update(dt);
end;

procedure TGame.Render;
  var WV, P: TUMat;
  var WindowSize: TPoint;
  var sx, sy: Single;
  var bg: TUVec3;
  var i: Int32;
begin
  bg := TUColor.Make(79, 190, 255).AsVec3;
  glClearColor(bg.x, bg.y, bg.z, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  WindowSize := Window.Size;
  if (WindowSize.x < 1) or (WindowSize.y < 1) then Exit;
  if (WindowSize.X > WindowSize.Y) then
  begin
    sx := WindowSize.Y / WindowSize.X;
    sy := 1;
  end
  else
  begin
    sx := 1;
    sy := WindowSize.X / WindowSize.Y;
  end;
  WV := TUMat.Identity;
  P := TUMat.Make(
    0.1 * sx, 0, 0, 0,
    0, 0.1 * sy, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );
  glMatrixMode(GL_MODELVIEW);
  glLoadMatrixf(@WV);
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(@P);
  glBegin(GL_QUADS);
  for i := 0 to High(Barriers) do
  begin
    Barriers[i].Render;
  end;
  if Crash.Crashed then
  begin
    Crash.Render;
  end;
  Bird.Render;
  glEnd();
  PrintScore;
  if not Playing then PrintStart;
end;

procedure TGame.Reset;
  var i: Int32;
begin
  if Score > HighScore then HighScore := Score;
  Playing := False;
  Score := 0;
  Bird.Initialize;
  Crash.Initialize;
  for i := 0 to High(Barriers) do
  begin
    Barriers[i].Initialize;
    Barriers[i].p.x := (PlayArea div 2) + i * (PlayArea div Length(Barriers));
    Barriers[i].Reset;
  end;
  Game.Audio.PlaySound(123, 60 + Random(20) - 10, 80);
  ResetDelay := 2;
end;

procedure TGame.PrintScore;
  var p: TUVec2;
  var s: Single;
  var t: String;
begin
  if Game.Playing then
  begin
    s := 0.05;
    t := 'SCORE: ' + UIntToStr(Score);
    p := TUVec2.Make(-(FontB.TextWidth(t) * s * 0.5), 10);
    PrintOutlined(p, TUVec2.Make(s, s), t, $ffffffff, True, 2);
  end
  else if HighScore > 0 then
  begin
    s := 0.04;
    t := 'HIGH SCORE: ' + UIntToStr(HighScore);
    p := TUVec2.Make(-12, 9.7);
    PrintOutlined(p, TUVec2.Make(s, s), t, $ffffffff, True);
  end;
end;

procedure TGame.PrintStart;
  var p: TUVec2;
  var s: Single;
  var t: String;
  var c: TUColor;
  var a: Single;
begin
  s := 0.05;
  t := 'START';
  p := TUVec2.Make(-(FontB.TextWidth(t) * s * 0.5), FontB.TextHeight('A') * 0.5 * s);
  a := abs(sin(pi * 2 * ((GetTickCount64 mod 2000) / 2000)));
  c := TUColor.Make(255, 255, 255, Trunc(a * 255));
  PrintOutlined(p, TUVec2.Make(s, s), t, c, True);
end;

function TGame.WasKeyPressed(): Boolean;
begin
  Result := Window.KeyPressed;
end;

function TGame.MakeRotation(const a: Single): TUVec2;
  var s, c: Single;
begin
  USinCos(a, s, c);
  Result := TUVec2.Make(c, s);
end;

function TGame.RotateVec2(const v, r: TUVec2): TUVec2;
begin
  Result := TUVec2.Make(
    v.x * r.x - v.y * r.y,
    v.x * r.y + v.y * r.x
  );
end;

function TGame.ProjToRect(const v: TUVec2; const r: TUVec4): TUVec2;
begin
  Result := TUVec2.Make(
    UClamp(v.x, r.x, r.z),
    UClamp(v.y, r.y, r.w)
  );
end;

procedure TGame.DrawRect(const TopLeft, BottomRight: TUVec2; const Color: TUColor);
  var c: TUVec4;
begin
  c := Color.AsVec4;
  glColor4fv(@c);
  glVertex2f(TopLeft.x, TopLeft.y);
  glVertex2f(BottomRight.x, TopLeft.y);
  glVertex2f(BottomRight.x, BottomRight.y);
  glVertex2f(TopLeft.x, BottomRight.y);
end;

procedure TGame.DrawBorder(const r: TUVec4; const s: Single; const c0, c1: TUColor);
  var cv0, cv1: TUVec4;
  var x0, x1, x2, x3, y0, y1, y2, y3: Single;
begin
  cv0 := c0.AsVec4;
  cv1 := c1.AsVec4;
  x0 := r.x - s;
  x1 := r.x;
  x2 := r.z;
  x3 := r.z + s;
  y0 := r.y - s;
  y1 := r.y;
  y2 := r.w;
  y3 := r.w + s;
  glColor4fv(@cv1);
  glVertex2f(x0, y3);
  glVertex2f(x0, y0);
  glColor4fv(@cv0);
  glVertex2f(x1, y1);
  glVertex2f(x1, y2);
  glColor4fv(@cv1);
  glVertex2f(x0, y0);
  glVertex2f(x3, y0);
  glColor4fv(@cv0);
  glVertex2f(x2, y1);
  glVertex2f(x1, y1);
  glColor4fv(@cv1);
  glVertex2f(x3, y0);
  glVertex2f(x3, y3);
  glColor4fv(@cv0);
  glVertex2f(x2, y2);
  glVertex2f(x2, y1);
  glColor4fv(@cv1);
  glVertex2f(x3, y3);
  glVertex2f(x0, y3);
  glColor4fv(@cv0);
  glVertex2f(x1, y2);
  glVertex2f(x2, y2);
end;

procedure TGame.DrawQuad(const v0, v1, v2, v3: TUVec2; const Color: TUColor);
  var col: TUVec4;
begin
  col := Color.AsVec4;
  glColor4fv(@col);
  glVertex2fv(@v0);
  glVertex2fv(@v1);
  glVertex2fv(@v2);
  glVertex2fv(@v3);
end;

procedure TGame.DrawQuadCol(const v0, v1, v2, v3: TUVec2; const c0, c1, c2, c3: TUColor);
  var cv0, cv1, cv2, cv3: TUVec4;
begin
  cv0 := c0.AsVec4;
  cv1 := c1.AsVec4;
  cv2 := c2.AsVec4;
  cv3 := c3.AsVec4;
  glColor4fv(@cv0);
  glVertex2fv(@v0);
  glColor4fv(@cv1);
  glVertex2fv(@v1);
  glColor4fv(@cv2);
  glVertex2fv(@v2);
  glColor4fv(@cv3);
  glVertex2fv(@v3);
end;

procedure TGame.DrawCircle(const c: TUVec2; const r: Single; const Color: TUColor);
  var col: TUVec4;
  var rot, v0, v1: TUVec2;
  var i: Int32;
  const seg = 16;
begin
  col := Color.AsVec4;
  glColor4fv(@col);
  rot := MakeRotation((pi * 2) / seg);
  v1 := TUVec2.Make(r, 0);
  for i := 0 to seg - 1 do
  begin
    v0 := v1;
    v1 := RotateVec2(v0, rot);
    glVertex2f(c.x, c.y);
    glVertex2f(c.x + v0.x, c.y + v0.y);
    glVertex2f(c.x + v1.x, c.y + v1.y);
    glVertex2f(c.x, c.y);
  end;
end;

procedure TGame.DrawLine(const p0, p1: TUVec2; const s: Single; const Color: TUColor);
  var d, n: TUVec2;
begin
  d := p1 - p0;
  if d.Len < UEps then
  begin
    d := TUVec2.Make(1, 0);
  end;
  d := d.Norm * (s * 0.5);
  n := TUVec2.Make(-d.y, d.x);
  DrawQuad(
    p0 - d - n, p0 - d + n,
    p1 + d + n, p1 + d - n,
    Color
  );
end;

procedure TGame.PrintOutlined(
  const p: TUVec2; const s: TUVec2; const Text: String; const Color: TUColor;
  const Bold: Boolean; const OutlineScale: Single
);
  var f: TFont;
  var os: TUVec2;
  var ci: TUColor;
begin
  if Bold then f := FontB else f := FontN;
  ci := Color.Inverse;
  ci.a := Color.a;
  os := s * OutlineScale;
  f.Print(p + TUVec2.Make(-os.x, 0), s, Text, ci);
  f.Print(p + TUVec2.Make(os.x, 0), s, Text, ci);
  f.Print(p + TUVec2.Make(0, -os.y), s, Text, ci);
  f.Print(p + TUVec2.Make(0, os.y), s, Text, ci);
  f.Print(p, s, Text, Color);
end;

class procedure TGame.Main;
begin
  Game := TGame.Create;
  Game.Initialize;
  Game.Loop;
  Game.Finalize;
  Game.Free;
end;

end.

