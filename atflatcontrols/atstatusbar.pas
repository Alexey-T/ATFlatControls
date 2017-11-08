{
ATStatusBar component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL
}

unit ATStatusBar;

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows,
  Messages,
  {$endif}
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls;

type

  { TATStatusData }

  TATStatusData = class
  public
    ItemWidth: integer;
    ItemAlign: TAlignment;
    ItemCaption: string;
    ItemImageIndex: integer;
    ItemFontColor: integer;
    ItemBackColor: integer;
    constructor Create; virtual;
  end;

type
  TATStatusClickEvent = procedure (Sender: TObject; AIndex: integer) of object;
  TATStatusDrawEvent = procedure (Sender: TObject; AIndex: integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

const
  cDefStatusbarPadding = 1;
  cDefStatusbarColorBack = $E0E0E0;
  cDefStatusbarColorBorderTop = clGray;
  cDefStatusbarColorBorderR = clGray;
  cDefStatusbarColorBorderL = clNone;
  cDefStatusbarColorBorderU = clNone;
  cDefStatusbarColorBorderD = clNone;

type
  { TATStatus }

  TATStatus = class(TCustomControl)
  private
    FColorBorderTop: TColor;
    FColorBorderR: TColor;
    FColorBorderL: TColor;
    FColorBorderU: TColor;
    FColorBorderD: TColor;
    FPadding: integer;
    FClickedIndex: integer;
    FScalePercents: integer;

    FList: TList;
    FBitmap: TBitmap;
    FImages: TImageList;

    FOnPanelClick: TATStatusClickEvent;
    FOnPanelDrawBefore: TATStatusDrawEvent;
    FOnPanelDrawAfter: TATStatusDrawEvent;

    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintPanelTo(C: TCanvas; ARect: TRect; AData: TATStatusData);
    function IsIndexOk(AIndex: integer): boolean;
    function DoDrawBefore(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoDrawAfter(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function GetCaption(AIndex: integer): string;
    procedure SetCaption(AIndex: integer; const AValue: string);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    function GetPanelRect(AIndex: integer): TRect;
    function GetPanelAt(X, Y: integer): integer;
    function GetPanelData(AIndex: integer): TATStatusData;
    function PanelCount: integer;
    procedure AddPanel(AWidth: integer; AAlign: TAlignment;
      const ACaption: string=''; AImageIndex: integer=-1);
    procedure DeletePanel(AIndex: integer);
    procedure DeletePanels;
    property Captions[AIndex: integer]: string read GetCaption write SetCaption; default;
    procedure DoPanelAutosize(AIndex: integer);
    property ScalePercents: integer read FScalePercents write FScalePercents default 100;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property DoubleBuffered;
    property Enabled;
    property Visible;
    property ParentColor;
    property Font;
    property Color default cDefStatusbarColorBack;
    property ColorBorderTop: TColor read FColorBorderTop write FColorBorderTop default cDefStatusbarColorBorderTop;
    property ColorBorderR: TColor read FColorBorderR write FColorBorderR default cDefStatusbarColorBorderR;
    property ColorBorderL: TColor read FColorBorderL write FColorBorderL default cDefStatusbarColorBorderL;
    property ColorBorderU: TColor read FColorBorderU write FColorBorderU default cDefStatusbarColorBorderU;
    property ColorBorderD: TColor read FColorBorderD write FColorBorderD default cDefStatusbarColorBorderD;
    property Padding: integer read FPadding write FPadding default cDefStatusbarPadding;
    property Images: TImageList read FImages write FImages;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnResize;
    property OnPanelClick: TATStatusClickEvent read FOnPanelClick write FOnPanelClick;
    property OnPanelDrawBefore: TATStatusDrawEvent read FOnPanelDrawBefore write FOnPanelDrawBefore;
    property OnPanelDrawAfter: TATStatusDrawEvent read FOnPanelDrawAfter write FOnPanelDrawAfter;
  end;

implementation

uses
  SysUtils, Forms, Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

{ TATStatusData }

constructor TATStatusData.Create;
begin
  ItemAlign:= taLeftJustify;
  ItemImageIndex:= -1;
  ItemWidth:= 100;
  ItemFontColor:= clNone;
  ItemBackColor:= clNone;
end;

{ TATStatus }

function TATStatus.IsIndexOk(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FList.Count);
end;

function TATStatus.PanelCount: integer;
begin
  Result:= FList.Count;
end;

constructor TATStatus.Create(AOnwer: TComponent);
begin
  inherited;

  Align:= alBottom;
  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;

  Width:= 400;
  Height:= 24;

  FScalePercents:= 100;
  FPadding:= cDefStatusbarPadding;

  Color:= cDefStatusbarColorBack;
  FColorBorderTop:= cDefStatusbarColorBorderTop;
  FColorBorderR:= cDefStatusbarColorBorderR;
  FColorBorderL:= cDefStatusbarColorBorderL;
  FColorBorderU:= cDefStatusbarColorBorderU;
  FColorBorderD:= cDefStatusbarColorBorderD;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FList:= TList.Create;
end;

destructor TATStatus.Destroy;
var
  i: integer;
begin
  for i:= PanelCount-1 downto 0 do
  begin
    TObject(FList[i]).Free;
    FList[i]:= nil;
  end;
  FreeAndNil(FList);

  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATStatus.Paint;
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

procedure TATStatus.DoPaintPanelTo(C: TCanvas; ARect: TRect; AData: TATStatusData);
var
  RectText: TRect;
  PosIcon: TPoint;
  TextSize: TSize;
  NOffsetLeft: integer;
begin
  if AData.ItemBackColor<>clNone then
    C.Brush.Color:= AData.ItemBackColor
  else
    C.Brush.Color:= Color;
  C.FillRect(ARect);

  RectText:= Rect(ARect.Left+FPadding, ARect.Top, ARect.Right-FPadding, ARect.Bottom);

  if Assigned(FImages) then
    if AData.ItemImageIndex>=0 then
    begin
      if AData.ItemCaption='' then
        case AData.ItemAlign of
          taLeftJustify:
            PosIcon.x:= ARect.Left+FPadding;
          taRightJustify:
            PosIcon.x:= (ARect.Right-FImages.Width-FPadding);
          taCenter:
            PosIcon.x:= (ARect.Left+ARect.Right-FImages.Width) div 2
        end
      else
        PosIcon.x:= ARect.Left+FPadding;
      PosIcon.y:= (ARect.Top+ARect.Bottom-FImages.Height) div 2;

      FImages.Draw(C, PosIcon.x, PosIcon.y, AData.ItemImageIndex);
      Inc(RectText.Left, FImages.Width);
    end;

  if AData.ItemCaption<>'' then
  begin
    C.FillRect(RectText);
    TextSize:= C.TextExtent(AData.ItemCaption);

    case AData.ItemAlign of
      taLeftJustify:
        NOffsetLeft:= FPadding;
      taRightJustify:
        NOffsetLeft:= RectText.Right-RectText.Left-TextSize.cx - FPadding*2;
      taCenter:
        NOffsetLeft:= (RectText.Right-RectText.Left-TextSize.cx) div 2 - FPadding;
    end;

    if AData.ItemFontColor<>clNone then
      C.Font.Color:= AData.ItemFontColor
    else
      C.Font.Color:= Self.Font.Color;

    ExtTextOut(C.Handle,
      RectText.Left+NOffsetLeft+2,
      (ARect.Top+ARect.Bottom-TextSize.cy) div 2+1,
      ETO_CLIPPED+ETO_OPAQUE,
      @RectText,
      PChar(AData.ItemCaption),
      Length(AData.ItemCaption),
      nil);
  end;

  if FColorBorderR<>clNone then
  begin
    C.Pen.Color:= FColorBorderR;
    C.MoveTo(ARect.Right, ARect.Top);
    C.LineTo(ARect.Right, ARect.Bottom);
  end;

  if FColorBorderL<>clNone then
  begin
    C.Pen.Color:= FColorBorderL;
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Left, ARect.Bottom);
  end;

  if FColorBorderU<>clNone then
  begin
    C.Pen.Color:= FColorBorderU;
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Right, ARect.Top);
  end;

  if FColorBorderD<>clNone then
  begin
    C.Pen.Color:= FColorBorderD;
    C.MoveTo(ARect.Left, ARect.Bottom-1);
    C.LineTo(ARect.Right, ARect.Bottom-1);
  end;  
end;

function TATStatus.GetPanelRect(AIndex: integer): TRect;
var
  i: integer;
begin
  Result.Left:= 0;
  Result.Right:= -1;
  Result.Top:= 1;
  Result.Bottom:= ClientHeight;

  if IsIndexOk(AIndex) then
    for i:= 0 to PanelCount-1 do
    begin
      Result.Left:= Result.Right + 1;
      Result.Right:= Result.Left + TATStatusData(FList[i]).ItemWidth - 1;
      if AIndex=i then Exit;
    end;
end;

procedure TATStatus.DoPaintTo(C: TCanvas);
var
  i: integer;
  ARect: TRect;
begin
  C.Brush.Color:= Color;
  C.FillRect(ClientRect);
  C.Font.Assign(Self.Font);

  for i:= 0 to PanelCount-1 do
  begin
    ARect:= GetPanelRect(i);
    if DoDrawBefore(i, C, ARect) then
    begin
      DoPaintPanelTo(C, ARect, TATStatusData(FList[i]));
      DoDrawAfter(i, C, ARect);
    end;  
  end;

  C.Pen.Color:= FColorBorderTop;
  C.MoveTo(0, 0);
  C.LineTo(ClientWidth, 0);
end;


function TATStatus.GetPanelAt(X, Y: integer): integer;
var
  i: integer;
  Pnt: TPoint;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  for i:= 0 to PanelCount-1 do
    if PtInRect(GetPanelRect(i), Pnt) then
    begin
      Result:= i;
      Exit;
    end;
end;

procedure TATStatus.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  FClickedIndex:= GetPanelAt(X, Y);
end;

function TATStatus.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATStatus.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, Width);
    FBitmap.Height:= Max(FBitmap.Height, Height);
  end;
  Invalidate;
end;


procedure TATStatus.AddPanel(AWidth: integer; AAlign: TAlignment;
  const ACaption: string = '';
  AImageIndex: integer=-1);
var
  Data: TATStatusData;
begin
  Data:= TATStatusData.Create;
  Data.ItemWidth:= MulDiv(AWidth, ScalePercents,  100);
  Data.ItemAlign:= AAlign;
  Data.ItemCaption:= ACaption;
  Data.ItemImageIndex:= AImageIndex;
  FList.Add(Data);
  Invalidate;
end;

procedure TATStatus.DeletePanel(AIndex: integer);
begin
  if IsIndexOk(AIndex) then
  begin
    TObject(FList[AIndex]).Free;
    FList.Delete(AIndex);
    Invalidate;
  end;
end;

procedure TATStatus.DeletePanels;
begin
  while PanelCount>0 do
    DeletePanel(PanelCount-1);
end;

function TATStatus.GetPanelData(AIndex: integer): TATStatusData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATStatusData(FList[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATStatus.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATStatus.Click;
begin
  inherited;
  if Assigned(FOnPanelClick) then
    FOnPanelClick(Self, FClickedIndex);
end;

function TATStatus.DoDrawBefore(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawBefore) then
    FOnPanelDrawBefore(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.DoDrawAfter(AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawAfter) then
    FOnPanelDrawAfter(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.GetCaption(AIndex: integer): string;
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
    Result:= D.ItemCaption
  else
    Result:= '';
end;

procedure TATStatus.SetCaption(AIndex: integer; const AValue: string);
var
  D: TATStatusData;
begin
  D:= GetPanelData(AIndex);
  if Assigned(D) then
  begin
    D.ItemCaption:= AValue;
    Invalidate;
  end;
end;


procedure TATStatus.DoPanelAutosize(AIndex: integer);
var
  NSize, i: integer;
  D: TATStatusData;
begin
  if not IsIndexOk(AIndex) then exit;

  NSize:= 0;
  for i:= 0 to PanelCount-1 do
    if i<>AIndex then
    begin
      D:= GetPanelData(i);
      if Assigned(D) then
        Inc(NSize, D.ItemWidth);
    end;

  D:= GetPanelData(AIndex);
  if Assigned(D) then
    D.ItemWidth:= Max(0, Width-NSize);
end;


end.
