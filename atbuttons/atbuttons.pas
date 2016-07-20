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
  { TATButton }

  TATButton = class(TCustomControl)
  private
    FPressed,
    FOver,
    FChecked,
    FCheckable,
    FFocusable: boolean;
    FCaption: string;
    FBitmap: TPicture;
    FOnClick: TNotifyEvent;
    FImageList: TImageList;
    FImageIndex: integer;
    FFlat: boolean;
    procedure DoClick;
    function IsPressed: boolean;
    procedure SetCaption(AValue: string);
    procedure SetChecked(AValue: boolean);
    procedure SetFocusable(AValue: boolean);
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
    property TabStop;
    property TabOrder;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property Caption: string read FCaption write SetCaption;
    property Bitmap: TPicture read FBitmap write FBitmap;
    property Checked: boolean read FChecked write SetChecked;
    property Checkable: boolean read FCheckable write FCheckable;
    property ImageList: TImageList read FImageList write FImageList;
    property ImageIndex: integer read FImageIndex write FImageIndex;
    property Focusable: boolean read FFocusable write SetFocusable;
    property Flat: boolean read FFlat write FFlat;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses Math, Types;

{ TATButton }

procedure TATButton.SetChecked(AValue: boolean);
begin
  if FChecked= AValue then Exit;
  FChecked:= AValue;
  Invalidate;
end;

procedure TATButton.SetFocusable(AValue: boolean);
begin
  if FFocusable= AValue then Exit;
  FFocusable:= AValue;
  TabStop:= AValue;
end;

procedure TATButton.SetCaption(AValue: string);
begin
  if FCaption= AValue then Exit;
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
  p: TPoint;
  size, i: integer;
begin
  inherited;

  if not FFlat or FOver then
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
  if FCaption<>'' then
  begin
    Canvas.Font.Name:= ATButtonTheme.FontName;
    Canvas.Font.Color:= IfThen(Enabled, ATButtonTheme.ColorFont, ATButtonTheme.ColorFontDisabled);
    Canvas.Font.Size:= ATButtonTheme.FontSize;
    Canvas.Font.Style:= ATButtonTheme.FontStyles;

    p.x:= (ClientWidth - Canvas.TextWidth(FCaption)) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight - Canvas.TextHeight(FCaption)) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    Canvas.TextOut(p.x, p.y, FCaption);
  end;

  //----draw ImageList icon
  if Assigned(FImageList) and
    (FImageIndex>=0) and
    (FImageIndex<FImageList.Count) then
  begin
    p.x:= (ClientWidth-FImageList.Width) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight-FImageList.Height) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    FImageList.Draw(Canvas, p.x, p.y, FImageIndex);
    exit
  end;

  //----draw bitmap
  if Assigned(FBitmap) then
  begin
    p.x:= (ClientWidth-FBitmap.Width) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight-FBitmap.Height) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    Canvas.Draw(p.x, p.y, FBitmap.Graphic);
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
  FBitmap:= TPicture.Create;
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FFocusable:= true;
  FFlat:= false;
  FOnClick:= nil;
  FImageList:= nil;
  FImageIndex:= -1;
end;

destructor TATButton.Destroy;
begin
  FBitmap.Free;

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
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
  end;

end.

