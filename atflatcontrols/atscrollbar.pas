{
ATScrollBar for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL

Features:
- fully supports owner-draw of all elements (arrows, backgnd, thumb, corner empty area)
- prop: border size
- prop: arrow mark size
- prop: size of corner empty area (for additional controls maybe)
- prop: kind of arrows (normal, both above, both below, no arrows)

Mouse usage:
- click and holding mouse on arrows
- click and holding mouse on page-up (area above thumb) / page-down (area below thumb)
- dragging of thumb
}

unit ATScrollBar;

{$mode delphi}

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Forms;

type
  TATScrollbarElemType = (
    aseArrowUp,
    aseArrowDown,
    aseArrowLeft,
    aseArrowRight,
    aseScrollThumbV,
    aseScrollThumbH,
    aseScrollAreaH,
    aseScrollAreaV,
    aseScrolledAreaH,
    aseScrolledAreaV,
    aseCorner
    );

type
  TATScrollbarArrowsStyle = (
    asaArrowsNormal,
    asaArrowsBelow,
    asaArrowsAbove,
    asaArrowsHidden
    );

type
  TATScrollbarDrawEvent = procedure (Sender: TObject; AType: TATScrollbarElemType;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  PATScrollbarTheme = ^TATScrollbarTheme;
  TATScrollbarTheme = record
    ColorBG: TColor;
    ColorBorder: TColor;
    ColorThumbBorder: TColor;
    ColorThumbFill: TColor;
    ColorArrowBorder: TColor;
    ColorArrowFill: TColor;
    ColorArrowSign: TColor;
    ColorScrolled: TColor;
    InitialSize: integer;
    ScalePercents: integer;
    ArrowSize: integer;
    ArrowLengthPercents: integer;
    BorderSize: integer;
    TimerInterval: integer;
    ThumbMarkerOffset: integer;
    ThumbMarkerMinimalSize: integer;
    ThumbMarkerDecorSize: integer;
  end;

var
  ATScrollbarTheme: TATScrollbarTheme;

type
  { TATScrollbar }

  TATScrollbar = class(TCustomControl)
  private
    FKind: TScrollBarKind;
    FArrowStyle: TATScrollbarArrowsStyle;
    FIndentCorner: Integer;
    FTheme: PATScrollbarTheme;

    FPos: Integer;
    FMin: Integer;
    FMax: Integer;
    FLineSize: Integer;
    FPageSize: Integer;
    FMinSizeToShowThumb: Integer;
    FMinSizeOfThumb: Integer;

    //internal
    FRectMain: TRect; //area for scrolling
    FRectArrUp: TRect; //area for up or left arrow
    FRectArrDown: TRect; //area for down or right arrow
    FRectThumb: TRect; //area for scroll-thumb
    FRectCorner: TRect;
    FRectPageUp: TRect;
    FRectPageDown: TRect;

    FBitmap: TBitmap;
    FTimer: TTimer;
    FOnChange: TNotifyEvent;
    FOnOwnerDraw: TATScrollbarDrawEvent;

    //drag-drop
    FMouseDown: boolean;
    FMouseDragOffset: Integer;
    FMouseDownOnUp,
    FMouseDownOnDown,
    FMouseDownOnThumb,
    FMouseDownOnPageUp,
    FMouseDownOnPageDown: boolean;

    procedure DoPaintArrow(C: TCanvas; const R: TRect; Typ: TATScrollbarElemType);
    procedure DoPaintThumb(C: TCanvas);
    procedure DoPaintBack(C: TCanvas);
    procedure DoPaintBackScrolled(C: TCanvas);
    procedure DoPaintTo(C: TCanvas);

    procedure DoPaintStd_Corner(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Back(C: TCanvas; const R: TRect);
    procedure DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Arrow(C: TCanvas; R: TRect; Typ: TATScrollbarElemType);
    procedure DoPaintStd_Thumb(C: TCanvas; const R: TRect);

    function IsHorz: boolean; inline;
    function MouseToPos(X, Y: Integer): Integer;
    procedure DoUpdateThumbRect;
    procedure DoUpdateCornerRect;
    procedure DoUpdatePosOnDrag(X, Y: Integer);
    procedure DoScrollBy(NDelta: Integer);
    function GetPxAtScroll(APos: Integer): Integer;
    function DoScale(AValue: integer): integer;

    procedure TimerTimer(Sender: TObject);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetArrowStyle(AValue: TATScrollbarArrowsStyle);
    procedure SetPos(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPageSize(Value: Integer);
    function DoDrawEvent(AType: TATScrollbarElemType;
      ACanvas: TCanvas; const ARect: TRect): boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    property Theme: PATScrollbarTheme read FTheme write FTheme;
    procedure Update; reintroduce;

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property DoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property Position: Integer read FPos write SetPos default 0;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property LineSize: Integer read FLineSize write FLineSize default 1;
    property PageSize: Integer read FPageSize write SetPageSize default 20;
    property MinSizeToShowThumb: Integer read FMinSizeToShowThumb write FMinSizeToShowThumb default 10;
    property MinSizeOfThumb: Integer read FMinSizeOfThumb write FMinSizeOfThumb default 4;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property ArrowStyle: TATScrollbarArrowsStyle read FArrowStyle write SetArrowStyle default asaArrowsNormal;
    property IndentCorner: Integer read FIndentCorner write FIndentCorner default 0;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnOwnerDraw: TATScrollbarDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
    property OnContextPopup;
    property OnResize;
  end;

implementation

uses
  SysUtils, Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

{ TATScrollbar }

constructor TATScrollbar.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];

  FKind:= sbHorizontal;
  FArrowStyle:= asaArrowsNormal;
  FIndentCorner:= 0;

  FTheme:= @ATScrollbarTheme;
  Width:= 200;
  Height:= FTheme.InitialSize;
  Color:= FTheme^.ColorBG;

  DoubleBuffered:= IsDoubleBufferedNeeded;

  FMin:= 0;
  FMax:= 100;
  FLineSize:= 1;
  FPageSize:= 20;
  FMinSizeToShowThumb:= 10;
  FMinSizeOfThumb:= 4;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= false;
  FTimer.Interval:= 100;
  FTimer.OnTimer:= TimerTimer;

  FMouseDown:= false;
  FMouseDragOffset:= 0;
end;

destructor TATScrollbar.Destroy;
begin
  FTimer.Enabled:= false;
  FreeAndNil(FTimer);
  FreeAndNil(FBitmap);
  inherited;
end;

function TATScrollbar.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATScrollbar.Update;
begin
  if IsHorz then
    Height:= DoScale(FTheme^.InitialSize)
  else
    Width:= DoScale(FTheme^.InitialSize);

  Invalidate;
end;

function TATScrollbar.DoScale(AValue: integer): integer;
begin
  Result:= AValue * FTheme^.ScalePercents div 100;
end;

procedure TATScrollbar.Paint;
begin
  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
    begin
      DoPaintTo(FBitmap.Canvas);
      Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
    end;
  end
  else
    DoPaintTo(Canvas);
end;

procedure TATScrollbar.DoPaintTo(C: TCanvas);
var
  FSize: Integer;
begin
  FRectMain:= ClientRect;
  FRectArrUp:= Rect(0, 0, 0, 0);
  FRectArrDown:= Rect(0, 0, 0, 0);

  DoUpdateCornerRect;
  if not IsRectEmpty(FRectCorner) then
    if DoDrawEvent(aseCorner, C, FRectCorner) then
      DoPaintStd_Corner(C, FRectCorner);

  C.Brush.Color:= ColorToRGB(FTheme^.ColorBorder);
  C.FillRect(FRectMain);

  InflateRect(FRectMain,
    -DoScale(FTheme^.BorderSize),
    -DoScale(FTheme^.BorderSize)
    );

  if IsHorz then
  begin
    //horz kind
    FSize:= Math.Min(
      FRectMain.Height * FTheme^.ArrowLengthPercents div 100,
      FRectMain.Width div 2
      );
    case FArrowStyle of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Left, FSize);
          Dec(FRectMain.Right, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Left+FSize, FRectMain.Bottom);
          FRectArrDown:= Rect(FRectMain.Left+FSize, FRectMain.Top, FRectMain.Left+2*FSize, FRectMain.Bottom);
          Inc(FRectMain.Left, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrDown:= Rect(FRectMain.Right-FSize, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
          FRectArrUp:= Rect(FRectMain.Right-2*FSize, FRectMain.Top, FRectMain.Right-FSize, FRectMain.Bottom);
          Dec(FRectMain.Right, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowLeft);
    DoPaintArrow(C, FRectArrDown, aseArrowRight);
  end
  else
  begin
    //vertical kind
    FSize:= Math.Min(
      FRectMain.Width * FTheme^.ArrowLengthPercents div 100,
      FRectMain.Height div 2
      );
    case FArrowStyle of
      asaArrowsNormal:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Inc(FRectMain.Top, FSize);
          Dec(FRectMain.Bottom, FSize);
        end;
      asaArrowsBelow:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Bottom-2*FSize, FRectMain.Right, FRectMain.Bottom-FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Bottom-FSize, FRectMain.Right, FRectMain.Bottom);
          Dec(FRectMain.Bottom, 2*FSize);
        end;
      asaArrowsAbove:
        begin
          FRectArrUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+FSize);
          FRectArrDown:= Rect(FRectMain.Left, FRectMain.Top+FSize, FRectMain.Right, FRectMain.Top+2*FSize);
          Inc(FRectMain.Top, 2*FSize);
        end;
    end;
    DoPaintArrow(C, FRectArrUp, aseArrowUp);
    DoPaintArrow(C, FRectArrDown, aseArrowDown);
  end;

  DoPaintBack(C);
  DoUpdateThumbRect;
  DoPaintBackScrolled(C);
  DoPaintThumb(C);
end;

procedure TATScrollbar.DoPaintBack(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsHorz then Typ:= aseScrollAreaH else Typ:= aseScrollAreaV;
  if DoDrawEvent(Typ, C, FRectMain) then
    DoPaintStd_Back(C, FRectMain);
end;

procedure TATScrollbar.DoPaintBackScrolled(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsHorz then Typ:= aseScrolledAreaH else Typ:= aseScrolledAreaV;

  if FMouseDown and FMouseDownOnPageUp then
    if DoDrawEvent(Typ, C, FRectPageUp) then
      DoPaintStd_BackScrolled(C, FRectPageUp);

  if FMouseDown and FMouseDownOnPageDown then
    if DoDrawEvent(Typ, C, FRectPageDown) then
      DoPaintStd_BackScrolled(C, FRectPageDown);
end;


procedure TATScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= Button=mbLeft;
  FMouseDownOnThumb:= PtInRect(FRectThumb, Point(X, Y));
  FMouseDownOnUp:= PtInRect(FRectArrUp, Point(X, Y));
  FMouseDownOnDown:= PtInRect(FRectArrDown, Point(X, Y));
  FMouseDownOnPageUp:= PtInRect(FRectPageUp, Point(X, Y));
  FMouseDownOnPageDown:= PtInRect(FRectPageDown, Point(X, Y));

  if IsHorz then
    FMouseDragOffset:= X-FRectThumb.Left
  else
    FMouseDragOffset:= Y-FRectThumb.Top;

  FTimer.Interval:= FTheme^.TimerInterval;
  FTimer.Enabled:= FMouseDown and
    (FMouseDownOnUp or
     FMouseDownOnDown or
     FMouseDownOnPageUp or
     FMouseDownOnPageDown);
end;

procedure TATScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= false;
  FMouseDownOnThumb:= false;
  FTimer.Enabled:= false;
  Invalidate;
end;

procedure TATScrollbar.Resize;
begin
  inherited;

  if Assigned(FBitmap) then
  begin
    //little complicated to speed up
    if IsHorz then
    begin
      FBitmap.Width:= Math.Max(FBitmap.Width, Width);
      FBitmap.Height:= Height;
    end
    else
    begin
      FBitmap.Width:= Width;
      FBitmap.Height:= Math.Max(FBitmap.Height, Height);
    end;
  end;

  Invalidate;
end;


{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATScroll.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATScrollbar.Click;
begin
  inherited;
end;

function TATScrollbar.DoDrawEvent(AType: TATScrollbarElemType;
  ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnOwnerDraw) then
    FOnOwnerDraw(Self, AType, ACanvas, ARect, Result);
end;

procedure TATScrollbar.SetKind(AValue: TScrollBarKind);
begin
  if AValue=FKind then Exit;
  FKind:= AValue;
  Invalidate;
end;

procedure TATScrollbar.SetArrowStyle(AValue: TATScrollbarArrowsStyle);
begin
  if FArrowStyle=AValue then Exit;
  FArrowStyle:= AValue;
  Invalidate;
end;

procedure TATScrollbar.DoPaintArrow(C: TCanvas; const R: TRect;
  Typ: TATScrollbarElemType);
begin
  if IsRectEmpty(R) then exit;
  if DoDrawEvent(Typ, C, R) then
    DoPaintStd_Arrow(C, R, Typ);
end;    

procedure TATScrollbar.DoPaintStd_Arrow(C: TCanvas; R: TRect;
  Typ: TATScrollbarElemType);
var
  P, P1, P2, P3: TPoint;
  cc: Integer;
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowBorder);
  C.FillRect(R);

  InflateRect(R, -1, -1);
  C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowFill);
  C.FillRect(R);

  P:= CenterPoint(R);
  cc:= DoScale(FTheme^.ArrowSize);

  case Typ of
    aseArrowUp:
      begin
        P1:= Point(P.X-cc, P.Y+cc div 2);
        P2:= Point(P.X+cc, P.Y+cc div 2);
        P3:= Point(P.X, P.Y-cc+cc div 2);
      end;
    aseArrowDown:
      begin
        P1:= Point(P.X-cc, P.Y-cc div 2);
        P2:= Point(P.X+cc, P.Y-cc div 2);
        P3:= Point(P.X, P.Y+cc-cc div 2);
      end;
    aseArrowLeft:
      begin
        P1:= Point(P.X+cc div 2, P.Y-cc);
        P2:= Point(P.X+cc div 2, P.Y+cc);
        P3:= Point(P.X-cc+cc div 2, P.Y);
      end;
    aseArrowRight:
      begin
        P1:= Point(P.X-cc div 2    -1, P.Y-cc);
        P2:= Point(P.X-cc div 2    -1, P.Y+cc);
        P3:= Point(P.X+cc-cc div 2 -1, P.Y);
      end;
    else
      Exit;
 end;     

  C.Brush.Color:= ColorToRGB(FTheme^.ColorArrowSign);
  C.Pen.Color:= ColorToRGB(FTheme^.ColorArrowSign);
  C.Polygon([P1, P2, P3]);
end;

function TATScrollbar.IsHorz: boolean; inline;
begin
  Result:= FKind=sbHorizontal;
end;

function TATScrollbar.GetPxAtScroll(APos: Integer): Integer;
var
  N0, NLen: Integer;
begin
  if IsHorz then
  begin
    N0:= FRectMain.Left;
    NLen:= FRectMain.Width
  end
  else
  begin
    N0:= FRectMain.Top;
    NLen:= FRectMain.Height;
  end;
  Result:= N0 + (APos-FMin) * NLen div Math.Max(1, FMax-FMin);
end;

procedure TATScrollbar.DoUpdateThumbRect;
var
  R: TRect;
begin
  FRectThumb:= Rect(0, 0, 0, 0);
  FRectPageUp:= Rect(0, 0, 0, 0);
  FRectPageDown:= Rect(0, 0, 0, 0);

  if IsHorz then
  begin
    if FRectMain.Width<FMinSizeToShowThumb then Exit;
    R.Top:= FRectMain.Top;
    R.Bottom:= FRectMain.Bottom;
    R.Left:= GetPxAtScroll(FPos);
    R.Left:= Math.Min(R.Left, FRectMain.Right-FMinSizeOfThumb);
    R.Right:= GetPxAtScroll(FPos+FPageSize);
    R.Right:= Math.Max(R.Right, R.Left+FMinSizeOfThumb);
    R.Right:= Math.Min(R.Right, FRectMain.Right);
  end
  else
  begin
    if FRectMain.Height<FMinSizeToShowThumb then Exit;
    R.Left:= FRectMain.Left;
    R.Right:= FRectMain.Right;
    R.Top:= GetPxAtScroll(FPos);
    R.Top:= Math.Min(R.Top, FRectMain.Bottom-FMinSizeOfThumb);
    R.Bottom:= GetPxAtScroll(FPos+FPageSize);
    R.Bottom:= Math.Max(R.Bottom, R.Top+FMinSizeOfThumb);
    R.Bottom:= Math.Min(R.Bottom, FRectMain.Bottom);
  end;
  FRectThumb:= R;

  if IsHorz then
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectThumb.Left, FRectMain.Bottom);
    FRectPageDown:= Rect(FRectThumb.Right, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
  end
  else
  begin
    FRectPageUp:= Rect(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectThumb.Top);
    FRectPageDown:= Rect(FRectMain.Left, FRectThumb.Bottom, FRectMain.Right, FRectMain.Bottom);
  end;
end;

procedure TATScrollbar.DoPaintThumb(C: TCanvas);
var
  Typ: TATScrollbarElemType;
begin
  if IsRectEmpty(FRectThumb) then Exit;
  if IsHorz then
    Typ:= aseScrollThumbH
  else
    Typ:= aseScrollThumbV;

  if DoDrawEvent(Typ, C, FRectThumb) then
    DoPaintStd_Thumb(C, FRectThumb);
end;

procedure TATScrollbar.DoPaintStd_Thumb(C: TCanvas; const R: TRect);
var
  P: TPoint;
  NOffset, i: integer;
begin
  C.Brush.Color:= ColorToRGB(FTheme^.ColorThumbFill);
  C.Pen.Color:= ColorToRGB(FTheme^.ColorThumbBorder);
  C.Rectangle(R);

  NOffset:= FTheme^.ThumbMarkerOffset;

  P:= CenterPoint(R);
  if IsHorz then
  begin
    if R.Width>FTheme^.ThumbMarkerMinimalSize then
    begin
      for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
      begin
        C.MoveTo(P.X-2*i, R.Top+NOffset);
        C.LineTo(P.X-2*i, R.Bottom-NOffset);
        if i>0 then
        begin
          C.MoveTo(P.X+2*i, R.Top+NOffset);
          C.LineTo(P.X+2*i, R.Bottom-NOffset);
        end;
      end;
    end;
  end
  else
  begin
    if R.Height>FTheme^.ThumbMarkerMinimalSize then
    begin
      for i:= 0 to FTheme^.ThumbMarkerDecorSize-1 do
      begin
        C.MoveTo(R.Left+NOffset, P.Y-2*i);
        C.LineTo(R.Right-NOffset, P.Y-2*i);
        if i>0 then
        begin
          C.MoveTo(R.Left+NOffset, P.Y+2*i);
          C.LineTo(R.Right-NOffset, P.Y+2*i);
        end;
      end;
    end;
  end;
end;


procedure TATScrollbar.SetMax(Value: Integer);
begin
  if FMax<>Value then
  begin
    FMax:= Value;
    FPos:= Math.Min(FPos, FMax);
    Invalidate;
  end;
end;

procedure TATScrollbar.SetMin(Value: Integer);
begin
  if FMin<>Value then
  begin
    FMin:= Value;
    FPos:= Math.Max(FPos, FMin);
    Invalidate;
  end;
end;

procedure TATScrollbar.SetPageSize(Value: Integer);
begin
  if FPageSize<>Value then
  begin
    FPageSize:= Value;
    Invalidate;
  end;
end;

procedure TATScrollbar.SetPos(Value: Integer);
begin
  Value:= Math.Min(Value, FMax);
  Value:= Math.Max(Value, FMin);
  if FPos<>Value then
  begin
    FPos:= Value;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TATScrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDownOnThumb then
  begin
    DoUpdatePosOnDrag(X, Y);
    Exit
  end;
end;

function TATScrollbar.MouseToPos(X, Y: Integer): Integer;
begin
  if IsHorz then
    Result:= FMin + (X-FRectMain.Left) * (FMax-FMin) div Math.Max(FRectMain.Width, 1)
  else
    Result:= FMin + (Y-FRectMain.Top) * (FMax-FMin) div Math.Max(FRectMain.Height, 1);
end;

procedure TATScrollbar.DoUpdatePosOnDrag(X, Y: Integer);
var
  N: Integer;
begin
  N:= MouseToPos(
    X-FMouseDragOffset,
    Y-FMouseDragOffset);
  N:= Math.Max(N, FMin);
  N:= Math.Min(N, FMax-FPageSize);
  SetPos(N);
end;

procedure TATScrollbar.DoScrollBy(NDelta: Integer);
var
  N: Integer;
begin
  N:= FPos;
  Inc(N, NDelta);
  if (NDelta>0) then
    N:= Math.Min(N, FMax-FPageSize);
  SetPos(N);
end;

procedure TATScrollbar.TimerTimer(Sender: TObject);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  P:= ScreenToClient(P);

  if FMouseDownOnDown and PtInRect(FRectArrDown, P) then
    DoScrollBy(FLineSize)
  else
  if FMouseDownOnUp and PtInRect(FRectArrUp, P) then
    DoScrollBy(-FLineSize)
  else
  if FMouseDownOnPageDown and PtInRect(FRectPageDown, P) then
    DoScrollBy(FPageSize)
  else
  if FMouseDownOnPageUp and PtInRect(FRectPageUp, P) then
    DoScrollBy(-FPageSize);
end;

procedure TATScrollbar.DoPaintStd_Corner(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorBG);
  C.FillRect(R);
end;

procedure TATScrollbar.DoPaintStd_Back(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorBG);
  C.FillRect(R);
end;

procedure TATScrollbar.DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
begin
  if IsRectEmpty(R) then exit;
  C.Brush.Color:= ColorToRGB(FTheme^.ColorScrolled);
  C.FillRect(R);
end;

procedure TATScrollbar.DoUpdateCornerRect;
var
  Delta: integer;
begin
  FRectCorner:= Rect(0, 0, 0, 0);

  if IsHorz then
    Delta:= FIndentCorner * Height div 100
  else
    Delta:= FIndentCorner * Width div 100;

  if IsHorz then
  begin
    if Delta>0 then
    begin
      FRectCorner:= Rect(ClientWidth-Delta, 0, ClientWidth, ClientHeight);
      Dec(FRectMain.Right, Delta);
    end
    else
    if Delta<0 then
    begin
      FRectCorner:= Rect(0, 0, Abs(Delta), ClientHeight);
      Inc(FRectMain.Left, Abs(Delta));
    end;
  end
  else
  begin
    if Delta>0 then
    begin
      FRectCorner:= Rect(0, ClientHeight-Delta, ClientWidth, ClientHeight);
      Dec(FRectMain.Bottom, Delta);
    end
    else
    if Delta<0 then
    begin
      FRectCorner:= Rect(0, 0, ClientWidth, Abs(Delta));
      Inc(FRectMain.Top, Abs(Delta));
    end;
  end;
end;

initialization

  with ATScrollbarTheme do
  begin
    ColorBG:= $d0d0d0;
    ColorBorder:= clLtGray;
    ColorThumbBorder:= $808080;
    ColorThumbFill:= $c0c0c0;
    ColorArrowBorder:= $808080;
    ColorArrowFill:= $c0c0c0;
    ColorArrowSign:= $404040;
    ColorScrolled:= $d0b0b0;
    InitialSize:= 16;
    ScalePercents:= 100;
    ArrowSize:= 3;
    ArrowLengthPercents:= 100;
    BorderSize:= 0;
    TimerInterval:= 80;
    ThumbMarkerOffset:= 4;
    ThumbMarkerMinimalSize:= 20;
    ThumbMarkerDecorSize:= 2;
  end;

end.
