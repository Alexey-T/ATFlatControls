{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Menus,
  Types, Math, Forms,
  LCLType;

type
  TATButtonTheme = record
    FontName: string;
    FontSize: integer;
    FontStyles: TFontStyles;
    ColorFont,
    ColorFontDisabled,
    ColorBgPassive,
    ColorBgOver,
    ColorBgChecked,
    ColorBgDisabled,
    ColorArrows,
    ColorBorderPassive,
    ColorBorderOver,
    ColorBorderFocused: TColor;
    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
    ChoiceLeftIndent: integer;
    BoldBorderWidth: integer;
    ChoiceBorderWidth: integer;
  end;

var
  ATButtonTheme: TATButtonTheme;

type
  TATButtonKind = (
    abuTextOnly,
    abuIconOnly,
    abuTextIconHorz,
    abuTextIconVert,
    abuSeparatorHorz,
    abuSeparatorVert,
    abuTextChoice
    );

const
  cATButtonKindValues: array[TATButtonKind] of string = (
    'text',
    'icon',
    'text_icon_h',
    'text_icon_v',
    'sep_h',
    'sep_v',
    'text_choice'
    );

type
  { TATButton }

  TATButton = class(TCustomControl)
  private
    FPressed,
    FOver,
    FChecked,
    FCheckable,
    FFocusable: boolean;
    FPicture: TPicture;
    FImages: TImageList;
    FImageIndex: integer;
    FArrow: boolean;
    FFlat: boolean;
    FKind: TATButtonKind;
    FBoldBorder: boolean;
    FBoldFont: boolean;
    FDataString: string;
    FDataString2: string;
    FDataString3: string;
    FItems: TStringList;
    FItemIndex: integer;
    FPopup: TPopupMenu;
    procedure DoChoiceClick(Sender: TObject);
    function GetIconHeight: integer;
    function GetIconWidth: integer;
    function IsPressed: boolean;
    procedure PaintBorder(C: TCanvas; R: TRect; AColor: TColor; AWidth: integer);
    procedure PaintIcon(AX, AY: integer);
    procedure PaintArrow(AX, AY: integer);
    procedure SetBoldFont(AValue: boolean);
    procedure SetChecked(AValue: boolean);
    procedure SetFlat(AValue: boolean);
    procedure SetFocusable(AValue: boolean);
    procedure SetImageIndex(AValue: integer);
    procedure SetImages(AValue: TImageList);
    procedure SetKind(AValue: TATButtonKind);
    procedure SetBoldBorder(AValue: boolean);
    procedure ShowChoiceMenu;
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    property DataString: string read FDataString write FDataString;
    property DataString2: string read FDataString2 write FDataString2;
    property DataString3: string read FDataString3 write FDataString3;
    function GetTextSize(const S: string): TSize;
    property Items: TStringList read FItems;
    property ItemIndex: integer read FItemIndex write FItemIndex;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property TabStop;
    property TabOrder;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Checked: boolean read FChecked write SetChecked default false;
    property Checkable: boolean read FCheckable write FCheckable default false;
    property Images: TImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Focusable: boolean read FFocusable write SetFocusable default true;
    property Flat: boolean read FFlat write SetFlat default false;
    property Arrow: boolean read FArrow write FArrow default false;
    property Kind: TATButtonKind read FKind write SetKind default abuTextOnly;
    property BoldBorder: boolean read FBoldBorder write SetBoldBorder default false;
    property BoldFont: boolean read FBoldFont write SetBoldFont default false;
    property Picture: TPicture read FPicture write FPicture;
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

var
  cATButtonArrowSize: integer = 2;
  cATButtonIndent: integer = 3;
  cATButtonIndentArrow: integer = 5;

implementation

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

{ TATButton }

procedure TATButton.SetChecked(AValue: boolean);
begin
  if FChecked=AValue then Exit;
  FChecked:= AValue;
  Invalidate;
end;

procedure TATButton.SetFlat(AValue: boolean);
begin
  if FFlat=AValue then Exit;
  FFlat:= AValue;
  Invalidate;
  if FFlat then
    Focusable:= false;
end;

procedure TATButton.SetFocusable(AValue: boolean);
begin
  if FFocusable=AValue then Exit;
  FFocusable:= AValue;
  TabStop:= AValue;
end;

procedure TATButton.SetImageIndex(AValue: integer);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:= AValue;
  Invalidate;
end;

procedure TATButton.SetImages(AValue: TImageList);
begin
  if FImages=AValue then Exit;
  FImages:= AValue;
  Invalidate;
end;

procedure TATButton.SetKind(AValue: TATButtonKind);
begin
  if FKind=AValue then Exit;
  FKind:= AValue;
  Invalidate;
end;

procedure TATButton.SetBoldBorder(AValue: boolean);
begin
  if FBoldBorder=AValue then Exit;
  FBoldBorder:= AValue;
  Invalidate;
end;

procedure TATButton.Click;
begin
  if FKind=abuTextChoice then
  begin
    ShowChoiceMenu;
    exit
  end;

  inherited;
  if FCheckable then
    FChecked:= not FChecked;
  Invalidate;
end;

function TATButton.CanFocus: boolean;
begin
  Result:= FFocusable;
end;

function TATButton.IsPressed: boolean;
begin
  Result:= FPressed and FOver;
end;

procedure TATButton.PaintBorder(C: TCanvas; R: TRect; AColor: TColor; AWidth: integer);
var
  i: integer;
begin
  C.Brush.Style:= bsClear;
  C.Pen.Color:= AColor;
  C.Rectangle(R);

  for i:= 1 to AWidth-1 do
  begin
    InflateRect(R, -1, -1);
    C.Rectangle(R);
  end;

  C.Brush.Style:= bsSolid;
end;

procedure TATButton.Paint;
var
  r: TRect;
  pnt1, pnt2: TPoint;
  NSize, dy, NSizeArrow, i: integer;
  bUseBack, bUseBorder: boolean;
  NColor: TColor;
  S: string;
begin
  inherited;

  if FArrow then
    NSizeArrow:= 4*cATButtonArrowSize
  else
    NSizeArrow:= 0;

  bUseBack:=
    (not FFlat)
    or FChecked
    or (FOver and not (FKind in [abuSeparatorHorz, abuSeparatorVert]));
  bUseBorder:= bUseBack
    or (FKind=abuTextChoice);

  r:= ClientRect;

  if bUseBack then
  begin
    Canvas.Brush.Color:=
      IfThen(not Enabled, ATButtonTheme.ColorBgDisabled,
       IfThen(FChecked, ATButtonTheme.ColorBgChecked,
        IfThen(FOver, ATButtonTheme.ColorBgOver, ATButtonTheme.ColorBgPassive)));
    Canvas.FillRect(r);
  end;

  if bUseBorder then
  begin
    if FOver then
      NColor:= ATButtonTheme.ColorBorderOver
    else
    if Focused then
      NColor:= ATButtonTheme.ColorBorderFocused
    else
      NColor:= ATButtonTheme.ColorBorderPassive;

    NSize:= 1;
    if IsPressed then
      NSize:= ATButtonTheme.PressedBorderWidth
    else
    if BoldBorder then
      NSize:= ATButtonTheme.BoldBorderWidth
    else
    if Kind=abuTextChoice then
      NSize:= ATButtonTheme.ChoiceBorderWidth
    else
    if FOver then
      NSize:= ATButtonTheme.MouseoverBorderWidth;

    PaintBorder(Canvas, R, NColor, NSize);
  end;

  Canvas.Font.PixelsPerInch:= Screen.PixelsPerInch;
  Canvas.Font.Name:= ATButtonTheme.FontName;
  Canvas.Font.Color:= IfThen(Enabled, ATButtonTheme.ColorFont, ATButtonTheme.ColorFontDisabled);
  Canvas.Font.Size:= ATButtonTheme.FontSize;
  if BoldFont then
    Canvas.Font.Style:= [fsBold]
  else
    Canvas.Font.Style:= ATButtonTheme.FontStyles;
  Canvas.Brush.Style:= bsClear;

  case FKind of
    abuIconOnly:
      begin
        pnt1.x:= (ClientWidth-GetIconWidth-NSizeArrow) div 2+
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        pnt1.y:= (ClientHeight-GetIconHeight) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        PaintIcon(pnt1.x, pnt1.y);
      end;

    abuTextOnly:
      begin
        pnt1.x:= (ClientWidth-GetTextSize(Caption).cx-NSizeArrow) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        pnt1.y:= (ClientHeight-GetTextSize(Caption).cy) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        Canvas.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextIconHorz:
      begin
        pnt1.x:= cATButtonIndent +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        pnt1.y:= (ClientHeight-GetIconHeight) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        PaintIcon(pnt1.x, pnt1.y);

        Inc(pnt1.x, GetIconWidth+cATButtonIndentArrow);
        pnt1.y:= (ClientHeight-GetTextSize(Caption).cy) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        Canvas.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextIconVert:
      begin
        pnt1.x:= (ClientWidth-GetIconWidth-NSizeArrow) div 2+
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        pnt1.y:= cATButtonIndent +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        PaintIcon(pnt1.x, pnt1.y);

        Inc(pnt1.y, GetIconHeight+cATButtonIndentArrow);
        pnt1.x:= (ClientWidth-GetTextSize(Caption).cx-NSizeArrow) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        Canvas.TextOut(pnt1.x, pnt1.y, Caption);
      end;

    abuTextChoice:
      begin
        pnt1.x:= ATButtonTheme.ChoiceLeftIndent +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
        pnt1.y:= (ClientHeight-GetTextSize('W').cy) div 2 +
          IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
        if (FItemIndex>=0) and (FItemIndex<FItems.Count) then
          S:= FItems[FItemIndex]
        else
          S:= '?';
        Canvas.TextOut(pnt1.x, pnt1.y, S);
      end;

    abuSeparatorVert:
      begin
        dy:= 2;
        pnt1:= Point(dy, Height div 2);
        pnt2:= Point(Width-dy, Height div 2);
        Canvas.Pen.Color:= ATButtonTheme.ColorArrows;
        Canvas.Line(pnt1, pnt2);
      end;

    abuSeparatorHorz:
      begin
        dy:= 2;
        pnt1:= Point(Width div 2, dy);
        pnt2:= Point(Width div 2, Height-dy);
        Canvas.Pen.Color:= ATButtonTheme.ColorArrows;
        Canvas.Line(pnt1, pnt2);
      end;
  end;

  if FArrow then
  begin
    //if caption not empty, paint on right side, else centered
    if (Caption<>'') or (ImageIndex>=0) then
      pnt1.x:= ClientWidth - Scale96ToScreen(cATButtonArrowSize*4)
    else
      pnt1.x:= ClientWidth div 2 - Scale96ToScreen(cATButtonArrowSize div 2);

    pnt1.y:= ClientHeight div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);

    PaintArrow(pnt1.x, pnt1.y);
  end;
end;

procedure TATButton.PaintIcon(AX, AY: integer);
begin
  if Assigned(FImages) and (FImageIndex>=0) and (FImageIndex<FImages.Count) then
    FImages.Draw(Canvas, AX, AY, FImageIndex)
  else
  if Assigned(FPicture) then
    Canvas.Draw(AX, AY, FPicture.Graphic);
end;

procedure TATButton.PaintArrow(AX, AY: integer);
var
  NSize: integer;
begin
  NSize:= Scale96ToScreen(cATButtonArrowSize);
  CanvasPaintTriangleDown(Canvas, ATButtonTheme.ColorArrows,
    Point(AX, AY), NSize);
end;

procedure TATButton.SetBoldFont(AValue: boolean);
begin
  if FBoldFont=AValue then Exit;
  FBoldFont:= AValue;
  Invalidate;
end;

function TATButton.GetIconWidth: integer;
begin
  if Assigned(FImages) then
    Result:= FImages.Width
  else
  if Assigned(FPicture) then
    Result:= FPicture.Width
  else
    Result:= 0;
end;

function TATButton.GetIconHeight: integer;
begin
  if Assigned(FImages) then
    Result:= FImages.Height
  else
  if Assigned(FPicture) then
    Result:= FPicture.Height
  else
    Result:= 0;
end;

procedure TATButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  bOver: boolean;
begin
  inherited;

  bOver:= PtInRect(ClientRect, Point(X, Y));
  if bOver<>FOver then
  begin
    FOver:= bOver;
    Invalidate;
  end;
end;

procedure TATButton.MouseLeave;
begin
  inherited;
  FOver:= false;
  Invalidate;
end;

procedure TATButton.MouseEnter;
begin
  inherited;
  FOver:= true;
  Invalidate;
end;

procedure TATButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Shift=[ssLeft] then
  begin
    FPressed:= true;
    if FFocusable then
      SetFocus;
  end;

  Invalidate;
end;

procedure TATButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FPressed:= false;
  Invalidate;
end;


procedure TATButton.KeyPress(var Key: char);
begin
  inherited;
  if (Key=' ') then
    Click;
end;

procedure TATButton.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TATButton.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TATButton.TextChanged;
begin
  inherited;
  Invalidate; //paint caption
end;

constructor TATButton.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque]
    -[csDoubleClicks, csTripleClicks];

  TabStop:= true;
  Width:= 100;
  Height:= 25;

  Caption:= 'Button';
  FPicture:= TPicture.Create;
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FFocusable:= true;
  FFlat:= false;
  FImages:= nil;
  FImageIndex:= -1;
  FKind:= abuTextOnly;
  FBoldBorder:= false;
  FItems:= TStringList.Create;
  FItemIndex:= -1;
end;

destructor TATButton.Destroy;
begin
  FItems.Free;
  FPicture.Free;

  inherited;
end;

function TATButton.GetTextSize(const S: string): TSize;
begin
  Result.cx:= 0;
  Result.cy:= 0;
  if S='' then exit;
  Canvas.Font.Name:= ATButtonTheme.FontName;
  Canvas.Font.Size:= ATButtonTheme.FontSize;
  if BoldFont then
    Canvas.Font.Style:= [fsBold]
  else
    Canvas.Font.Style:= ATButtonTheme.FontStyles;
  Result:= Canvas.TextExtent(S);
end;

procedure TATButton.DoChoiceClick(Sender: TObject);
begin
  FItemIndex:= (Sender as TComponent).Tag;
  Invalidate;
  inherited Click;
end;

procedure TATButton.ShowChoiceMenu;
var
  mi: TMenuItem;
  i: integer;
  P: TPoint;
begin
  if not Assigned(FPopup) then
    FPopup:= TPopupMenu.Create(Self);

  FPopup.Items.Clear;
  for i:= 0 to FItems.Count-1 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= FItems[i];
    mi.Tag:= i;
    mi.RadioItem:= true;
    mi.Checked:= i=FItemIndex;
    mi.OnClick:= @DoChoiceClick;
    FPopup.Items.Add(mi);
  end;

  P:= ClientToScreen(Point(0, Height));
  FPopup.PopUp(P.X, P.Y);
end;


initialization

  with ATButtonTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontStyles:= [];
    ColorFont:= $303030;
    ColorFontDisabled:= $808088;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $e0e0e0;
    ColorBgChecked:= $b0b0b0;
    ColorBgDisabled:= $c0c0d0;
    ColorArrows:= clGray;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
    ChoiceLeftIndent:= 5;
    BoldBorderWidth:= 3;
    ChoiceBorderWidth:= 1;
  end;

end.

