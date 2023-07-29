unit Externals;

{$mode ObjFPC}{$H+}

interface

type TPoint = record
  x, y: Int32;
end;

type TRect = record
  Left: Int32;
  Top: Int32;
  Right: Int32;
  Bottom: Int32;
end;
type PRect = ^TRect;

type TWndProc = function (_para1: THandle; _para2: UInt32; _para3: PtrInt; _para4:PtrInt): PtrInt; stdcall;

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;
                              PPGLvoid    = ^PGLvoid;

type TWndClassExA = record
  cbSize: UInt32;
  style: UInt32;
  lpfnWndProc: TWndProc;
  cbClsExtra: Longint;
  cbWndExtra: Longint;
  hInstance: THandle;
  hIcon: THandle;
  hCursor: THandle;
  hbrBackground: THandle;
  lpszMenuName: PChar;
  lpszClassName: PChar;
  hIconSm: THandle;
end;

type TPixelFormatDescriptor = record
  nSize: UInt16;
  nVersion: UInt16;
  dwFlags: UInt32;
  iPixelType: UInt8;
  cColorBits: UInt8;
  cRedBits: UInt8;
  cRedShift: UInt8;
  cGreenBits: UInt8;
  cGreenShift: UInt8;
  cBlueBits: UInt8;
  cBlueShift: UInt8;
  cAlphaBits: UInt8;
  cAlphaShift: UInt8;
  cAccumBits: UInt8;
  cAccumRedBits: UInt8;
  cAccumGreenBits: UInt8;
  cAccumBlueBits: UInt8;
  cAccumAlphaBits: UInt8;
  cDepthBits: UInt8;
  cStencilBits: UInt8;
  cAuxBuffers: UInt8;
  iLayerType: UInt8;
  bReserved: UInt8;
  dwLayerMask: UInt32;
  dwVisibleMask: UInt32;
  dwDamageMask: UInt32;
end;
type PPixelFormatDescriptor = ^TPixelFormatDescriptor;

type TMsg = record
  hwnd: THandle;
  message: UInt32;
  wParam: PtrInt;
  lParam: PtrInt;
  time: UInt32;
  pt: TPoint;
end;

type TBitmapInfoHeader = record
  biSize: UInt32;
  biWidth: Int32;
  biHeight: Int32;
  biPlanes: UInt16;
  biBitCount: UInt16;
  biCompression: UInt32;
  biSizeImage: UInt32;
  biXPelsPerMeter: Int32;
  biYPelsPerMeter: Int32;
  biClrUsed: UInt32;
  biClrImportant: UInt32;
end;

type TRGBQuad = record
  rgbBlue: UInt8;
  rgbGreen: UInt8;
  rgbRed: UInt8;
  rgbReserved: UInt8;
end;

type TBitmapInfo = record
  bmiHeader : TBitmapInfoHeader;
  bmiColors : array[0..0] of TRGBQuad;
end;

type TSize = record
  cx: Int32;
  cy: Int32;
end;

function GetTickCount64: UInt64; external 'kernel32' name 'GetTickCount64';
procedure Sleep(dwMilliseconds: UInt32); external 'kernel32' name 'Sleep';

function GetSystemMetrics(
  nIndex: longint
):longint; external 'user32' name 'GetSystemMetrics';
function GetDC(hWnd: THandle): THandle; external 'user32' name 'GetDC';
function ReleaseDC(
  hWnd: THandle; hDC: THandle
): Longint; external 'user32' name 'ReleaseDC';
function CreateWindowExA(
  dwExStyle: UInt32; lpClassName: PChar; lpWindowName: PChar; dwStyle: UInt32;
  X: Longint; Y: Longint; nWidth: Longint; nHeight: Longint; hWndParent: THandle;
  hMenu: THandle; hInstance: THandle; lpParam: Pointer
): THandle; external 'user32' name 'CreateWindowExA';
function SetWindowLong(
  hWnd: THandle; nIndex: Longint; dwNewLong: Longint
): Longint; external 'user32' name 'SetWindowLongA';
function GetWindowLong(
  hWnd: THandle; nIndex: Longint
): Longint; external 'user32' name 'GetWindowLongA';
function ShowWindow(
  hWnd: THandle; nCmdShow: Longint
): Longbool; external 'user32' name 'ShowWindow';
function BringWindowToTop(
  hWnd: THandle
): Longbool; external 'user32' name 'BringWindowToTop';
function DestroyWindow(
  hWnd: THandle
): Longbool; external 'user32' name 'DestroyWindow';
function GetClientRect(
  hWnd: THandle; var lpRect: TRect
): Longbool; external 'user32' name 'GetClientRect';
function DefWindowProc(
  hWnd: THandle; Msg: UInt32; wParam: PtrInt; lParam: PtrInt
): PtrInt; external 'user32' name 'DefWindowProcA';
function LoadIcon(
  hInstance: THandle; lpIconName: PChar
): THandle; external 'user32' name 'LoadIconA';
function LoadCursor(
  hInstance: THandle; lpCursorName: PChar
): THandle; external 'user32' name 'LoadCursorA';
function RegisterClassExA(
  const WndClass: TWndClassExA
): UInt16; external 'user32' name 'RegisterClassExA';
function UnregisterClassA(
  lpClassName: PChar; hInstance: THandle
): Longbool; external 'user32' name 'UnregisterClassA';
function PeekMessage(
  var lpMsg: TMsg; hWnd: THandle; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UInt32
): Longbool; external 'user32' name 'PeekMessageA';
function TranslateMessage(
  const lpMsg: TMsg
): Longbool; external 'user32' name 'TranslateMessage';
function DispatchMessage(
  const lpMsg: TMsg
): Longint;external 'user32' name 'DispatchMessageA';
function SetWindowTextA(
  hWnd: THandle; lpString: PChar
): Longbool; external 'user32' name 'SetWindowTextA';

function SetPixelFormat(
  DC: THandle; iPixelFormat:longint; ppfd: PPixelFormatDescriptor
): Longbool; external 'gdi32' name 'SetPixelFormat';
function SwapBuffers(
  DC: THandle
): Longbool; external 'gdi32' name 'SwapBuffers';
function ChoosePixelFormat(
  DC: THandle; p2: PPixelFormatDescriptor
): Integer; stdcall; external 'gdi32' name 'ChoosePixelFormat';
function CreateCompatibleDC(
  _para1: THandle
): THandle; external 'gdi32' name 'CreateCompatibleDC';
function SetMapMode(
  _para1: THandle; _para2: Int32
): Int32; external 'gdi32' name 'SetMapMode';
function CreateFontA(
  _para1: Int32; _para2: Int32; _para3: Int32; _para4: Int32; _para5: Int32;
  _para6: UInt32; _para7: UInt32; _para8: UInt32; _para9: UInt32; _para10: UInt32;
  _para11: UInt32; _para12: UInt32; _para13: UInt32; _para14: PChar
): THandle; external 'gdi32' name 'CreateFontA';
function SelectObject(
  _para1: THandle; _para2: THandle
): THandle; external 'gdi32' name 'SelectObject';
function GetTextExtentPoint32A(
  DC: THandle; Str: PChar; Count: Int32; var Size: TSize
): Longbool; external 'gdi32' name 'GetTextExtentPoint32A';
function CreateDIBSection(
  _para1: THandle; var _para2: TBitmapInfo; _para3: UInt32; var _para4: Pointer;
  _para5: THandle; _para6: UInt32
): THandle; external 'gdi32' name 'CreateDIBSection';
function SetTextColor(
  _para1: THandle; _para2: UInt32
): UInt32; external 'gdi32' name 'SetTextColor';
function SetBkColor(
  _para1: THandle; _para2: UInt32
): UInt32; external 'gdi32' name 'SetBkColor';
function SetTextAlign(
  _para1: THandle; _para2: UInt32
): UInt32; external 'gdi32' name 'SetTextAlign';
function ExtTextOut(
  _para1: THandle; _para2: Int32; _para3: Int32; _para4: UInt32;
  _para5: PRect; _para6: PChar; _para7: UInt32; _para8: PInt32
): Longbool; external 'gdi32' name 'ExtTextOutA';
function DeleteObject(
  _para1: THandle
): Longbool; external 'gdi32' name 'DeleteObject';
function DeleteDC(
  _para1: THandle
): Longbool; external 'gdi32' name 'DeleteDC';

function wglCreateContext(
  hdc: THandle
): THandle; external 'opengl32' name 'wglCreateContext';
function wglMakeCurrent(
  DC: THandle; glrc: THandle
): Longbool; external 'opengl32' name 'wglMakeCurrent';
function wglDeleteContext(
  glrc: THandle
): Longbool; external 'opengl32' name 'wglDeleteContext';
procedure glViewport(
  x, y: Int32; width, height: Int32
); stdcall; external 'opengl32.dll' name 'glViewport';
procedure glClearColor(
  red, green, blue, alpha: Single
); stdcall; external 'opengl32.dll' name 'glClearColor';
procedure glClearDepth(
  depth: Double
); stdcall; external 'opengl32.dll' name 'glClearDepth';
procedure glClear(
  mask: UInt32
); stdcall; external 'opengl32.dll' name 'glClear';
procedure glVertex2f(
  x, y: Single
); stdcall; external 'opengl32.dll' name 'glVertex2f';
procedure glVertex2fv(
  const v: PGLfloat
); stdcall; external 'opengl32.dll' name 'glVertex2fv';
procedure glColor4fv(
  const v: PGLfloat
); stdcall; external 'opengl32.dll' name 'glColor4fv';
procedure glTexCoord2f(
  s, t: GLfloat
); stdcall; external 'opengl32.dll' name 'glTexCoord2f';
procedure glMatrixMode(
  mode: GLenum
); stdcall; external 'opengl32.dll' name 'glMatrixMode';
procedure glLoadMatrixf(
  const m: PGLfloat
); stdcall; external 'opengl32.dll' name 'glLoadMatrixf';
procedure glBegin(
  mode: GLenum
); stdcall; external 'opengl32.dll' name 'glBegin';
function glGetError: GLenum; stdcall; external 'opengl32.dll' name 'glGetError';
procedure glEnd(); stdcall; external 'opengl32.dll' name 'glEnd';
procedure glPixelStorei(
  pname: GLenum; param: GLint
); stdcall; external 'opengl32.dll' name 'glPixelStorei';
procedure glGenTextures(
  n: GLsizei; textures: PGLuint
); stdcall; external 'opengl32.dll' name 'glGenTextures';
procedure glBindTexture(
  target: GLenum; texture: GLuint
); stdcall; external 'opengl32.dll' name 'glBindTexture';
procedure glTexImage2D(
  target: GLenum; level: GLInt; internalformat: GLEnum; width, height: GLsizei;
  border: GLint; format, atype: GLenum; const pixels: Pointer
); stdcall; external 'opengl32.dll' name 'glTexImage2D';
procedure glTexParameteri(
  target: GLenum; pname: GLenum; param: GLint
); stdcall; external 'opengl32.dll' name 'glTexParameteri';
procedure glDeleteTextures(
  n: GLsizei; const textures: PGLuint
); stdcall; external 'opengl32.dll' name 'glDeleteTextures';
procedure glBlendFunc(
  sfactor, dfactor: GLenum
); stdcall; external 'opengl32.dll' name 'glBlendFunc';
procedure glEnable(
  cap: GLenum
); stdcall; external 'opengl32.dll' name 'glEnable';
procedure glDisable(
  cap: GLenum
); stdcall; external 'opengl32.dll' name 'glDisable';

const WS_POPUP = $80000000;
const WS_VISIBLE = $10000000;
const WS_EX_TOPMOST = $8;
const WS_CAPTION = $c00000;
const WS_MINIMIZEBOX = $20000;
const WS_MAXIMIZEBOX = $10000;
const WS_SYSMENU = $80000;
const WS_THICKFRAME = $40000;
const WM_DESTROY = 2;
const WM_SIZE = 5;
const WM_CLOSE = 16;
const WM_QUIT = 18;
const WM_SETCURSOR = 32;
const WM_KEYDOWN = 256;
const WM_KEYUP = 257;
const WM_CHAR = 258;
const WM_LBUTTONDOWN = 513;
const WM_LBUTTONDBLCLK = 515;
const WM_LBUTTONUP = 514;
const WM_RBUTTONDOWN = 516;
const WM_MBUTTONDOWN = 519;
const WM_RBUTTONUP = 517;
const WM_RBUTTONDBLCLK = 518;
const WM_MBUTTONDBLCLK = 521;
const WM_MBUTTONUP = 520;
const WM_MOUSEWHEEL = 522;
const SW_NORMAL = 1;
const SM_CXSCREEN = 0;
const SM_CYSCREEN = 1;
const PFD_MAIN_PLANE = 0;
const PFD_DOUBLEBUFFER = $1;
const PFD_DRAW_TO_WINDOW = $4;
const PFD_SUPPORT_OPENGL = $20;
const PFD_TYPE_RGBA = 0;
const GWL_USERDATA = -(21);
const CS_VREDRAW = 1;
const CS_HREDRAW = 2;
const CS_DBLCLKS = 8;
const CS_OWNDC = 32;
const IDC_ARROW = PAnsiChar(32512);
const PM_REMOVE = 1;
const GL_MODELVIEW = $1700;
const GL_PROJECTION = $1701;
const GL_TEXTURE = $1702;
const GL_POINTS = $0000;
const GL_LINES = $0001;
const GL_LINE_LOOP = $0002;
const GL_LINE_STRIP = $0003;
const GL_TRIANGLES = $0004;
const GL_TRIANGLE_STRIP = $0005;
const GL_TRIANGLE_FAN = $0006;
const GL_QUADS = $0007;
const GL_QUAD_STRIP = $0008;
const GL_POLYGON = $0009;
const MM_TEXT = 1;
const FW_NORMAL = 400;
const FW_BOLD = 700;
const DEFAULT_CHARSET = 1;
const OUT_DEFAULT_PRECIS = 0;
const CLIP_DEFAULT_PRECIS = 0;
const NONANTIALIASED_QUALITY = 3;
const ANTIALIASED_QUALITY = 4;
const VARIABLE_PITCH = 2;
const DIB_RGB_COLORS = 0;
const BI_RGB = 0;
const TA_TOP = 0;
const ETO_OPAQUE = 2;

const GL_NO_ERROR = 0;
const GL_COLOR_BUFFER_BIT = $00004000;
const GL_UNPACK_ALIGNMENT = $0CF5;
const GL_TEXTURE_2D = $0DE1;
const GL_UNSIGNED_BYTE = $1401;
const GL_TEXTURE_MAG_FILTER = $2800;
const GL_TEXTURE_MIN_FILTER = $2801;
const GL_TEXTURE_WRAP_S = $2802;
const GL_TEXTURE_WRAP_T = $2803;
const GL_REPEAT = $2901;
const GL_NEAREST = $2600;
const GL_LINEAR = $2601;
const GL_RGB = $1907;
const GL_RGBA = $1908;
const GL_BLEND = $0BE2;
const GL_ZERO = 0;
const GL_ONE = 1;
const GL_SRC_COLOR = $0300;
const GL_ONE_MINUS_SRC_COLOR = $0301;
const GL_SRC_ALPHA = $0302;
const GL_ONE_MINUS_SRC_ALPHA = $0303;
const GL_DST_ALPHA = $0304;
const GL_ONE_MINUS_DST_ALPHA = $0305;

implementation

end.

