{
Copyright (C) Alexey Torgashin, uvviewsoft.com
Written for Lazarus LCL
License: MPL 2.0 or LGPL or any license which LCL can use
}

unit ATGauge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TGaugeKind = (
    gkText,
    gkHorizontalBar,
    gkVerticalBar,
    gkPie,
    gkNeedle,
    gkHalfPie
    );

const
  cInitGaugeValue = 20;
  cInitGaugeKind = gkHorizontalBar;

type
  { TGauge }

  TGauge = class(TGraphicControl)
  private
    FDoubleBuffered: boolean;
    FBitmap: TBitmap;
    FBorderStyle: TBorderStyle;
    FKind: TGaugeKind;
    FColorBack,
    FColorFore,
    FColorBorder: TColor;
    FMinValue,
    FMaxValue,
    FProgress: integer;
    FShowText: boolean;
    FShowTextInverted: boolean;
    procedure DoPaintTextInverted(C: TCanvas; r: TRect; const Str: string);
    procedure DoPaintTextUsual(C: TCanvas; r: TRect; const Str: string);
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function GetPercentDone: integer;
    function GetPartDoneFloat: Double;
    procedure SetColorBorder(AValue: TColor);
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetColorBack(AValue: TColor);
    procedure SetColorFore(AValue: TColor);
    procedure SetKind(AValue: TGaugeKind);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetProgress(AValue: integer);
    procedure SetShowText(AValue: boolean);
    procedure SetShowTextInverted(AValue: boolean);
  protected
    procedure Paint; override;
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress(AValue: integer);
    property PercentDone: integer read GetPercentDone;
  published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DoubleBuffered: boolean read FDoubleBuffered write FDoubleBuffered;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Kind: TGaugeKind read FKind write SetKind default cInitGaugeKind;
    property Progress: integer read FProgress write SetProgress default cInitGaugeValue;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property BackColor: TColor read FColorBack write SetColorBack default clWhite;
    property ForeColor: TColor read FColorFore write SetColorFore default clNavy;
    property BorderColor: TColor read FColorBorder write SetColorBorder default clBlack;
    property ShowText: boolean read FShowText write SetShowText default true;
    property ShowTextInverted: boolean read FShowTextInverted write SetShowTextInverted default false;
    property OnClick;
    property OnDblClick;
    property OnResize;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

uses
  Math, Types,
  InterfaceBase,
  LCLType, LCLIntf;

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;

{ TGauge }

procedure TGauge.DoPaintTextUsual(C: TCanvas; r: TRect; const Str: string);
var
  StrSize: TSize;
begin
  StrSize:= C.TextExtent(Str);
  C.Font.Assign(Self.Font);
  C.Brush.Style:= bsClear;
  C.TextOut(
    (r.Left+r.Right-StrSize.cx) div 2,
    (r.Top+r.Bottom-StrSize.cy) div 2,
    Str);
  C.Brush.Style:= bsSolid;
end;

procedure TGauge.DoPaintTextInverted(C: TCanvas; r: TRect; const Str: string);
const
  ColorEmpty = clBlack;
  ColorFont = clWhite;
var
  StrSize: TSize;
  Bmp: TBitmap;
  Pnt: TPoint;
begin
  StrSize:= C.TextExtent(Str);

  Bmp:= TBitmap.Create;
  try
    Bmp.PixelFormat:= pf24bit;
    Bmp.SetSize(StrSize.cx, StrSize.cy);
    Bmp.Transparent:= true;
    Bmp.TransparentColor:= ColorEmpty;

    Bmp.Canvas.Brush.Color:= ColorEmpty;
    Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);

    Bmp.Canvas.Font.Assign(Self.Font);
    Bmp.Canvas.Font.Color:= ColorFont;
    Bmp.Canvas.Font.Quality:= fqNonAntialiased; //antialias
    Bmp.Canvas.AntialiasingMode:= amOn; //antialias
    Bmp.Canvas.TextOut(0, 0, Str);

    Pnt.X:= (r.Left+r.Right-StrSize.cx) div 2;
    Pnt.Y:= (r.Top+r.Bottom-StrSize.cy) div 2;

    C.CopyMode:= cmSrcInvert;
    C.CopyRect(
      Rect(Pnt.X, Pnt.Y, Pnt.X+StrSize.cx, Pnt.Y+StrSize.cy),
      Bmp.Canvas,
      Rect(0, 0, StrSize.cx, StrSize.cy));
    C.CopyMode:= cmSrcCopy;
  finally
    Bmp.Free;
  end;
end;

procedure TGauge.DoPaintTo(C: TCanvas; r: TRect);
  //
  procedure DoFillBG(AColor: TColor);
  begin
    C.Pen.Color:= AColor;
    C.Brush.Color:= AColor;
    C.FillRect(r);
  end;
  //
var
  NSize: integer;
  Alfa: double;
  Str: string;
  r2: TRect;
begin
  case FKind of
    gkText:
      begin
        DoFillBG(FColorBack);
      end;

    gkHorizontalBar:
      begin
        DoFillBG(FColorBack);

        C.Brush.Color:= FColorFore;
        NSize:= Round((r.Right-r.Left) * GetPartDoneFloat);
        C.FillRect(r.Left, r.Top, r.Left+NSize, r.Bottom);
      end;

    gkVerticalBar:
      begin
        DoFillBG(FColorBack);

        C.Brush.Color:= FColorFore;
        NSize:= Round((r.Bottom-r.Top) * GetPartDoneFloat);
        C.FillRect(r.Left, r.Bottom-NSize, r.Right, r.Bottom);
      end;

    gkNeedle,
    gkHalfPie:
      begin
        DoFillBG(Color);

        if FBorderStyle<>bsNone then NSize:= 1 else NSize:= 0;
        r2:= Rect(
          r.Left+NSize, r.Top+NSize,
          r.Right-1-NSize, r.Bottom-1+(r.Bottom-r.Top)-NSize);

        C.Pen.Color:= FColorFore;
        C.Brush.Color:= FColorBack;
        C.Pie(r2.Left, r2.Top, r2.Right, r2.Bottom,
          r.Right, r.Bottom,
          r.Left, r.Bottom);

        if FKind=gkHalfPie then
          C.Brush.Color:= FColorFore;

        Alfa:= pi*GetPartDoneFloat;
        C.Pie(r2.Left, r2.Top, r2.Right, r2.Bottom,
          r2.Left-Round(1000*cos(Alfa)),
          r2.Bottom-Round(1000*sin(Alfa)),
          r2.Left,
          r2.Bottom);

        C.Pen.Color:= FColorFore;
        C.MoveTo(r2.Left, r.Bottom-1-NSize);
        C.LineTo(r2.Right, r.Bottom-1-NSize);
      end;

    gkPie:
      begin
        DoFillBG(Color);

        if FBorderStyle<>bsNone then NSize:= 1 else NSize:= 0;
        r2:= Rect(r.Left+NSize, r.Top+NSize, r.Right-NSize, r.Bottom-NSize);

        C.Pen.Color:= FColorFore;
        C.Brush.Color:= FColorBack;
        C.Ellipse(r2);

        Alfa:= 360*GetPartDoneFloat;
        C.Pen.Color:= FColorFore;
        C.Brush.Color:= FColorFore;
        C.RadialPie(r2.Left, r2.Top, r2.Right, r2.Bottom,
          16*90, //starting angle: 90 deg
          -Round(16*Alfa) //pie angle: -Alfa deg
         );
      end;
  end;

  //paint text
  if FShowText then
  begin
    Str:= IntToStr(PercentDone)+'%';
    if FShowTextInverted then
      DoPaintTextInverted(C, r, Str)
    else
      DoPaintTextUsual(C, r, Str);
  end;

  //paint border
  if FBorderStyle<>bsNone then
  begin
    C.Pen.Color:= FColorBorder;
    C.Brush.Style:= bsClear;
    C.Rectangle(r);
    C.Brush.Style:= bsSolid;
  end;
end;

function TGauge.GetPartDoneFloat: Double;
begin
  Result:= (FProgress-FMinValue) / (FMaxValue-FMinValue);
end;

function TGauge.GetPercentDone: integer;
begin
  Result:= Round(100 * GetPartDoneFloat);
end;

procedure TGauge.SetColorBorder(AValue: TColor);
begin
  if FColorBorder=AValue then Exit;
  FColorBorder:=AValue;
  Invalidate;
end;

procedure TGauge.SetBorderStyle(AValue: TBorderStyle);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  Invalidate;
end;

procedure TGauge.SetColorBack(AValue: TColor);
begin
  if FColorBack=AValue then Exit;
  FColorBack:=AValue;
  Invalidate;
end;

procedure TGauge.SetColorFore(AValue: TColor);
begin
  if FColorFore=AValue then Exit;
  FColorFore:=AValue;
  Invalidate;
end;

procedure TGauge.SetKind(AValue: TGaugeKind);
begin
  if FKind=AValue then Exit;
  FKind:=AValue;
  Invalidate;
end;

procedure TGauge.SetMaxValue(AValue: integer);
begin
  if FMaxValue=AValue then Exit;
  FMaxValue:=Max(FMinValue+1, AValue);
  FProgress:=Min(FProgress, FMaxValue);
  Invalidate;
end;

procedure TGauge.SetMinValue(AValue: integer);
begin
  if FMinValue=AValue then Exit;
  FMinValue:=Min(FMaxValue-1, AValue);
  FProgress:=Max(FProgress, FMinValue);
  Invalidate;
end;

procedure TGauge.SetProgress(AValue: integer);
begin
  if FProgress=AValue then Exit;
  FProgress:=Max(FMinValue, Min(FMaxValue, AValue));
  Invalidate;
end;

procedure TGauge.SetShowText(AValue: boolean);
begin
  if FShowText=AValue then Exit;
  FShowText:=AValue;
  Invalidate;
end;

procedure TGauge.SetShowTextInverted(AValue: boolean);
begin
  if FShowTextInverted=AValue then Exit;
  FShowTextInverted:=AValue;
  Invalidate;
end;

procedure TGauge.Paint;
var
  R: TRect;
begin
  inherited;

  R:= ClientRect;
  if DoubleBuffered then
  begin
    FBitmap.Canvas.Font.Assign(Self.Font);
    DoPaintTo(FBitmap.Canvas, R);
    Canvas.CopyRect(R, FBitmap.Canvas, R);
  end
  else
    DoPaintTo(Canvas, R);
end;


constructor TGauge.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque, csNoFocus]
    -[csDoubleClicks, csTripleClicks];

  Width:= 150;
  Height:= 50;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.SetSize(500, 80);

  FDoubleBuffered:= IsDoubleBufferedNeeded;
  FKind:= cInitGaugeKind;
  FBorderStyle:= bsSingle;

  Color:= clBtnFace;
  FColorBack:= clWhite;
  FColorFore:= clNavy;
  FColorBorder:= clBlack;

  FMinValue:= 0;
  FMaxValue:= 100;
  FProgress:= cInitGaugeValue;
  FShowText:= true;
  FShowTextInverted:= false;
end;

destructor TGauge.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TGauge.AddProgress(AValue: integer);
begin
  Progress:= Progress+AValue;
end;

procedure TGauge.DoOnResize;
const
  cResizeBitmapStep = 100;
var
  SizeX, SizeY: integer;
begin
  inherited;

  if DoubleBuffered then
  if Assigned(FBitmap) then
  begin
    SizeX:= (Width div cResizeBitmapStep + 1)*cResizeBitmapStep;
    SizeY:= (Height div cResizeBitmapStep + 1)*cResizeBitmapStep;
    if (SizeX>FBitmap.Width) or (SizeY>FBitmap.Height) then
    begin
      FBitmap.SetSize(SizeX, SizeY);
      FBitmap.FreeImage; //recommended, else seen black bitmap on bigsize
    end;
  end;

  Invalidate;
end;


initialization

end.

