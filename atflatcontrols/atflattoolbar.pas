{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATFlatToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls,
  ImgList, Menus, Math, ATButtons,
  LclType;

type
  { TATFlatToolbar }

  TATFlatToolbar = class(TCustomControl)
  private
    FImages: TImageList;
    FVertical: boolean;
    FScalePercents: integer;
    FButtonWidth: integer;
    FThemed: boolean; //for use in CudaText
    FWrapable: boolean;
    procedure PopupForDropdownClick(Sender: TObject);
    function GetButton(AIndex: integer): TATButton;
    function DoScale(N: integer): integer;
    procedure SetVertical(AValue: boolean);
    procedure SetWrapable(AValue: boolean);
    procedure UpdateAnchors;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: boolean; override;
    procedure AddButton(
      AImageIndex: integer;
      AOnClick: TNotifyEvent;
      const ACaption, AHint, ADataString: string;
      AShowCaption: boolean);
    procedure AddDropdown(
      AImageIndex: integer;
      AMenu: TPopupMenu;
      ADropdownEvent: TNotifyEvent=nil;
      const ACaption: string='';
      const AHint: string='';
      const ADataString: string='');
    procedure AddChoice(
      AOnClick: TNotifyEvent;
      AWidth: integer;
      AItems: TStrings;
      AItemIndex: integer;
      const AHint: string='';
      const ADataString: string='');
    procedure AddSep;
    procedure UpdateControls(AInvalidate: boolean=false);
    function ButtonCount: integer;
    function IsIndexOk(AIndex: integer): boolean;
    property Buttons[AIndex: integer]: TATButton read GetButton;
    property ScalePercents: integer read FScalePercents write FScalePercents default 100;
    property Themed: boolean read FThemed write FThemed;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property ButtonWidth: integer read FButtonWidth write FButtonWidth default 50;
    property Color;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentColor;
    property ParentShowHint;
    property Images: TImageList read FImages write FImages;
    property Vertical: boolean read FVertical write SetVertical default false;
    property Wrapable: boolean read FWrapable write SetWrapable default false;
  end;

implementation

{ TATFlatToolbar }

constructor TATFlatToolbar.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize:= false;
  Width:= 200;
  Height:= 20;
  Caption:= '';
  FImages:= nil;
  FVertical:= false;
  FScalePercents:= 100;
  FButtonWidth:= 50;
  FWrapable:= false;
end;

destructor TATFlatToolbar.Destroy;
begin
  inherited;
end;

function TATFlatToolbar.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATFlatToolbar.UpdateControls(AInvalidate: boolean);
var
  btn: TATButton;
  i: integer;
begin
  if ControlCount=0 then exit;

  for i:= ControlCount-1 downto 0 do
  begin
    btn:= Controls[i] as TATButton;

    if Vertical then
      btn.Width:= FButtonWidth
    else
    if Assigned(FImages) then
      btn.Height:= FImages.Height+2*btn.Padding;

    case btn.Kind of
      abuSeparatorVert:
        begin
          btn.Height:= 2*btn.PaddingBig;
        end;
      abuSeparatorHorz:
        begin
          btn.Width:= 2*btn.PaddingBig;
        end;

      abuIconOnly:
        begin
          if Vertical then
            btn.Height:= FImages.Height+2*btn.Padding
          else
            btn.Width:= FImages.Width+2*btn.Padding;
        end;

      abuTextOnly:
        begin
          if Vertical then
            btn.Height:= btn.GetTextSize(btn.Caption).cy+2*btn.Padding
          else
            btn.Width:= btn.GetTextSize(btn.Caption).cx+2*btn.Padding;
        end;

      abuTextIconVert:
        begin
          if Vertical then
            btn.Height:= btn.GetTextSize(btn.Caption).cy+FImages.Height+3*btn.Padding
          else
            btn.Width:= Max(btn.GetTextSize(btn.Caption).cx, FImages.Width)+2*btn.Padding;
        end;

      abuTextIconHorz:
        begin
          if Vertical then
            btn.Height:= Max(btn.GetTextSize(btn.Caption).cy, FImages.Height)+2*btn.Padding
          else
            btn.Width:= btn.GetTextSize(btn.Caption).cx+FImages.Width+3*btn.Padding;
        end;
    end;

    if btn.Arrow then
    begin
      if not Vertical then
        btn.Width:= btn.Width+4*ATButtonTheme.ArrowSize;
      if Vertical and (btn.Caption='') and (btn.ImageIndex<0) then
        btn.Height:= btn.Height+4*ATButtonTheme.ArrowSize;
    end;

    //scale
    btn.Height:= DoScale(btn.Height);
    if not Vertical then
      btn.Width:= DoScale(btn.Width);
  end;

  //anchor buttons in row
  UpdateAnchors;

  if AInvalidate then
    for i:= 0 to ControlCount-1 do
      Controls[i].Invalidate;
end;

procedure TATFlatToolbar.UpdateAnchors;
var
  CtlSource, Ctl: TControl;
  akind, akind2: TAnchorKind;
  i: integer;
begin
  if ControlCount=0 then exit;
  CtlSource:= Controls[0];
  CtlSource.Left:= 0;
  CtlSource.Top:= 0;

  for i:= 1 to ControlCount-1 do
  begin
    Ctl:= Controls[i];

    //Wrapable supported only for horiz kind
    if Wrapable and
      (not Vertical) and
      (Controls[i-1].Left + Controls[i-1].Width + Ctl.Width >= ClientWidth) then
    begin
      Ctl.AnchorSide[akLeft].Control:= CtlSource;
      Ctl.AnchorSide[akTop].Control:= CtlSource;
      Ctl.AnchorSide[akLeft].Side:= asrLeft;
      Ctl.AnchorSide[akTop].Side:= asrBottom;
      Ctl.Anchors:= [akLeft, akTop];
      CtlSource:= Ctl;
    end
    else
    begin
      if Vertical then
      begin
        akind:= akTop;
        akind2:= akLeft;
      end
      else
      begin
        akind:= akLeft;
        akind2:= akTop;
      end;

      Ctl.AnchorSide[akind].Control:= Controls[i-1];
      Ctl.AnchorSide[akind2].Control:= Controls[i-1];
      Ctl.AnchorSide[akind].Side:= asrRight;
      Ctl.AnchorSide[akind2].Side:= asrTop;
      Ctl.Anchors:= [akLeft, akTop];
    end;
  end;
end;

procedure TATFlatToolbar.Resize;
begin
  inherited;
  if Wrapable and not Vertical then
    UpdateAnchors;
end;

function TATFlatToolbar.ButtonCount: integer;
begin
  Result:= ControlCount;
end;

function TATFlatToolbar.IsIndexOk(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<ButtonCount);
end;

function TATFlatToolbar.GetButton(AIndex: integer): TATButton;
begin
  Result:= nil;
  if (AIndex>=0) and (AIndex<ControlCount) then
    if Controls[AIndex] is TATButton then
      Result:= Controls[AIndex] as TATButton;
end;

function TATFlatToolbar.DoScale(N: integer): integer;
begin
  if ScalePercents<=100 then
    Result:= N
  else
    Result:= MulDiv(N, ScalePercents, 100);
end;

procedure TATFlatToolbar.SetVertical(AValue: boolean);
begin
  if FVertical=AValue then Exit;
  FVertical:= AValue;
  UpdateControls;
end;

procedure TATFlatToolbar.SetWrapable(AValue: boolean);
begin
  if FWrapable=AValue then Exit;
  FWrapable:= AValue;
  UpdateControls;
end;

procedure TATFlatToolbar.AddButton(AImageIndex: integer;
  AOnClick: TNotifyEvent; const ACaption, AHint, ADataString: string;
  AShowCaption: boolean);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Focusable:= false;
  b.Flat:= true;
  b.Caption:= ACaption;
  b.Hint:= AHint;
  b.DataString:= ADataString;
  b.Images:= FImages;
  b.ImageIndex:= AImageIndex;
  b.ShowHint:= true;
  b.OnClick:= AOnClick;

  if not AShowCaption then
    b.Kind:= abuIconOnly
  else
  if ACaption='' then
    b.Kind:= abuIconOnly
  else
  if AImageIndex<0 then
    b.Kind:= abuTextOnly
  else
  begin
    if Vertical then
      b.Kind:= abuTextIconVert
    else
      b.Kind:= abuTextIconHorz;
  end;
end;

procedure TATFlatToolbar.AddDropdown(
  AImageIndex: integer;
  AMenu: TPopupMenu;
  ADropdownEvent: TNotifyEvent=nil;
  const ACaption: string='';
  const AHint: string='';
  const ADataString: string='');
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Images:= FImages;
  b.ImageIndex:= AImageIndex;
  b.Focusable:= false;
  b.Caption:= ACaption;
  b.Hint:= AHint;
  b.DataString:= ADataString;
  b.ShowHint:= true;
  b.Flat:= true;
  b.PopupMenu:= AMenu;
  b.Arrow:= true;
  if b.Caption='' then
    b.ArrowAlign:= taCenter
  else
    b.ArrowAlign:= taRightJustify;

  if ADropdownEvent=nil then
    b.OnClick:= @PopupForDropdownClick
  else
    b.OnClick:= ADropdownEvent;

  if AImageIndex>=0 then
  begin
    if b.Caption<>'' then
      if Vertical then
        b.Kind:= abuTextIconVert
      else
        b.Kind:= abuTextIconHorz
    else
      b.Kind:= abuIconOnly;
  end
  else
    b.Kind:= abuTextOnly;
end;

procedure TATFlatToolbar.AddChoice(AOnClick: TNotifyEvent;
  AWidth: integer; AItems: TStrings; AItemIndex: integer;
  const AHint: string; const ADataString: string);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Caption:= '';
  b.Width:= AWidth;
  b.OnClick:= AOnClick;
  b.Focusable:= false;
  b.Flat:= true;
  b.Arrow:= true;
  b.ArrowAlign:= taRightJustify;
  b.Kind:= abuTextChoice;
  b.Items.AddStrings(AItems);
  b.ItemIndex:= AItemIndex;
  b.Hint:= AHint;
  b.DataString:= ADataString;
end;

procedure TATFlatToolbar.AddSep;
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Focusable:= false;
  b.Caption:= '';
  b.Flat:= true;
  if Vertical then
    b.Kind:= abuSeparatorVert
  else
    b.Kind:= abuSeparatorHorz;
  b.Enabled:= false;
end;

procedure TATFlatToolbar.PopupForDropdownClick(Sender: TObject);
var
  C: TControl;
  P: TPoint;
begin
  C:= Sender as TControl;
  P:= C.ClientToScreen(Point(0, C.Height));
  C.PopupMenu.PopUp(P.X, P.Y);
end;


end.

