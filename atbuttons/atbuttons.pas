{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

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
  end;

var
  ATButtonTheme: TATButtonTheme;

type
  TATButtonSpecKind = (
    abkNone,
    abkDropdown,
    abkVerticalLine,
    abkCross
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
    FCaption: string;
    FDataString: string;
    FPicture: TPicture;
    FOnClick: TNotifyEvent;
    FImages: TImageList;
    FImageIndex: integer;
    FFlat: boolean;
    FShowCaption: boolean;
    FSpecKind: TATButtonSpecKind;
    procedure DoClick;
    function IsPressed: boolean;
    procedure SetCaption(AValue: string);
    procedure SetChecked(AValue: boolean);
    procedure SetFlat(AValue: boolean);
    procedure SetFocusable(AValue: boolean);
    procedure SetShowCaption(AValue: boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property TabStop;
    property TabOrder;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Caption: string read FCaption write SetCaption;
    property DataString: string read FDataString write FDataString;
    property Checked: boolean read FChecked write SetChecked default false;
    property Checkable: boolean read FCheckable write FCheckable default false;
    property Images: TImageList read FImages write FImages;
    property ImageIndex: integer read FImageIndex write FImageIndex default -1;
    property Focusable: boolean read FFocusable write SetFocusable default true;
    property Flat: boolean read FFlat write SetFlat default false;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
    property SpecKind: TATButtonSpecKind read FSpecKind write FSpecKind default abkNone;
    property Picture: TPicture read FPicture write FPicture;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
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
  cATButtonArrowSize: integer = 6;
  cATButtonArrowHorzIndent: integer = 5;

implementation

uses Math, Types;

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

procedure TATButton.SetShowCaption(AValue: boolean);
begin
  if FShowCaption=AValue then Exit;
  FShowCaption:= AValue;
  Invalidate;
end;

procedure TATButton.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:= AValue;
  Invalidate;
end;

function TATButton.IsPressed: boolean;
begin
  Result:= FPressed and FOver;
end;

procedure TATButton.Paint;
var
  r: TRect;
  p, p2, p3: TPoint;
  size, dx, dy, i: integer;
begin
  inherited;

  if (not FFlat) or (FOver and (FSpecKind<>abkVerticalLine)) then
  begin
    //----draw bg
    r:= ClientRect;
    Canvas.Brush.Color:=
      IfThen(not Enabled, ATButtonTheme.ColorBgDisabled,
       IfThen(FChecked, ATButtonTheme.ColorBgChecked,
        IfThen(FOver, ATButtonTheme.ColorBgOver, ATButtonTheme.ColorBgPassive)));
    Canvas.FillRect(r);

    //----draw border
    Canvas.Brush.Style:= bsClear;

    Canvas.Pen.Color:=
      IfThen(FOver, ATButtonTheme.ColorBorderOver,
        IfThen(Focused, ATButtonTheme.ColorBorderFocused, ATButtonTheme.ColorBorderPassive));
    Canvas.Rectangle(r);

    size:= 1;
    if IsPressed then size:= ATButtonTheme.PressedBorderWidth else
    if FOver then size:= ATButtonTheme.MouseoverBorderWidth;

    for i:= 1 to size-1 do
    begin
      InflateRect(r, -1, -1);
      Canvas.Rectangle(r);
    end;

    Canvas.Brush.Style:= bsSolid;
  end;

  //----draw caption
  case FSpecKind of
    abkNone,
    abkDropdown:
      begin
        if FShowCaption and (FCaption<>'') then
        begin
          Canvas.Font.Name:= ATButtonTheme.FontName;
          Canvas.Font.Color:= IfThen(Enabled, ATButtonTheme.ColorFont, ATButtonTheme.ColorFontDisabled);
          Canvas.Font.Size:= ATButtonTheme.FontSize;
          Canvas.Font.Style:= ATButtonTheme.FontStyles;
          Canvas.Brush.Style:= bsClear;

          if FSpecKind=abkNone then
            p.x:= (ClientWidth - Canvas.TextWidth(FCaption)) div 2 +
              IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX)
          else
            p.x:= cATButtonArrowHorzIndent;

          p.y:= (ClientHeight - Canvas.TextHeight(FCaption)) div 2 +
            IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
          Canvas.TextOut(p.x, p.y, FCaption);
        end;

        if FSpecKind=abkDropdown then
        begin
          dx:= Width - cATButtonArrowSize - cATButtonArrowHorzIndent;
          dy:= -cATButtonArrowSize div 4 - 1;
          p:= Point(dx, dy + Height div 2);
          p2:= Point(dx + cATButtonArrowSize, dy + Height div 2);
          p3:= Point(dx + cATButtonArrowSize div 2, dy + Height div 2 + cATButtonArrowSize div 2);
          Canvas.Brush.Style:= bsSolid;
          Canvas.Pen.Color:= ATButtonTheme.ColorArrows;
          Canvas.Brush.Color:= ATButtonTheme.ColorArrows;
          Canvas.Polygon([p, p2, p3]);
        end;
      end;

    abkVerticalLine:
      begin
        dy:= 2;
        p:= Point(Width div 2, dy);
        p2:= Point(Width div 2, Height-dy);
        Canvas.Pen.Color:= ATButtonTheme.ColorArrows;
        Canvas.Line(p, p2);
      end;

    abkCross:
      begin
        dx:= (Width-cATButtonArrowSize) div 2 - 1;
        dy:= (Height-cATButtonArrowSize) div 2 - 1;
        Canvas.Pen.Color:= ATButtonTheme.ColorArrows;
        Canvas.Line(dx, dy, dx+cATButtonArrowSize+1, dy+cATButtonArrowSize+1);
        Canvas.Line(dx+cATButtonArrowSize, dy, dx-1, dy+cATButtonArrowSize+1);
      end;
  end;

  //----draw ImageList icon
  if Assigned(FImages) and
    (FImageIndex>=0) and
    (FImageIndex<FImages.Count) then
  begin
    p.x:= (ClientWidth-FImages.Width) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight-FImages.Height) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    FImages.Draw(Canvas, p.x, p.y, FImageIndex);
    exit
  end;

  //----draw Picture
  if Assigned(FPicture) then
  begin
    p.x:= (ClientWidth-FPicture.Width) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight-FPicture.Height) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    Canvas.Draw(p.x, p.y, FPicture.Graphic);
  end;
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

  if IsPressed then
    DoClick;

  FPressed:= false;
  Invalidate;
end;

procedure TATButton.DoClick;
begin
  if FCheckable then
    FChecked:= not FChecked;
  Invalidate;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;


procedure TATButton.KeyPress(var Key: char);
begin
  inherited;
  if (Key=' ') then
    DoClick;
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

constructor TATButton.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque]
    -[csDoubleClicks, csTripleClicks];

  TabStop:= true;
  Width:= 100;
  Height:= 25;

  FCaption:= 'Button';
  FPicture:= TPicture.Create;
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FFocusable:= true;
  FFlat:= false;
  FOnClick:= nil;
  FImages:= nil;
  FImageIndex:= -1;
  FShowCaption:= true;
  FSpecKind:= abkNone;
end;

destructor TATButton.Destroy;
begin
  FPicture.Free;

  inherited;
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
  end;

end.

