{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtonsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls,
  ImgList, Menus, Math, ATButtons;

type
  { TATButtonsToolbar }

  TATButtonsToolbar = class(TCustomControl)
  private
    FImages: TImageList;
    FKindVertical: boolean;
    procedure PopupForDropdownClick(Sender: TObject);
    function GetButton(AIndex: integer): TATButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(
      AImageIndex: integer;
      AOnClick: TNotifyEvent;
      const ACaption, AHint, ADataString: string;
      AShowCaption: boolean);
    procedure AddDropdown(
      AMenu: TPopupMenu;
      ADropdownEvent: TNotifyEvent=nil;
      const ACaption: string='';
      const AHint: string='';
      const ADataString: string='');
    procedure AddSep;
    procedure UpdateControls;
    function ButtonCount: integer;
    property Buttons[AIndex: integer]: TATButton read GetButton;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentColor;
    property ParentShowHint;
    property Images: TImageList read FImages write FImages;
    property KindVertical: boolean read FKindVertical write FKindVertical default false;
  end;

implementation

{ TATButtonsToolbar }

constructor TATButtonsToolbar.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize:= true;
  Caption:= '';
  FImages:= nil;
  FKindVertical:= false;
end;

destructor TATButtonsToolbar.Destroy;
begin
  inherited;
end;

procedure TATButtonsToolbar.UpdateControls;
var
  btn: TATButton;
  akind: TAnchorKind;
  i: integer;
begin
  if ControlCount=0 then exit;

  for i:= ControlCount-1 downto 0 do
  begin
    btn:= Controls[i] as TATButton;

    if FKindVertical then
      btn.Width:= Self.Width
    else
    if Assigned(FImages) then
      btn.Height:= FImages.Height+2*cATButtonIndent;

    case btn.Kind of
      abuArrowOnly:
        begin
          if FKindVertical then
            btn.Height:= cATButtonArrowSize+2*cATButtonIndentArrow
          else
            btn.Width:= cATButtonArrowSize+2*cATButtonIndentArrow;
        end;

      abuSeparatorVert:
        begin
          btn.Height:= 2*cATButtonIndentArrow;
        end;
      abuSeparatorHorz:
        begin
          btn.Width:= 2*cATButtonIndentArrow;
        end;

      abuIconOnly:
        begin
          if FKindVertical then
            btn.Height:= FImages.Height+2*cATButtonIndentArrow
          else
            btn.Width:= FImages.Width+2*cATButtonIndentArrow;
        end;

      abuTextOnly:
        begin
          if FKindVertical then
            btn.Height:= btn.GetTextSize(btn.Caption).cy+2*cATButtonIndentArrow
          else
            btn.Width:= btn.GetTextSize(btn.Caption).cx+2*cATButtonIndentArrow;
        end;

      abuTextIconVert:
        begin
          if FKindVertical then
            btn.Height:= btn.GetTextSize(btn.Caption).cy+FImages.Height+3*cATButtonIndentArrow
          else
            btn.Width:= Max(btn.GetTextSize(btn.Caption).cx, FImages.Width)+2*cATButtonIndentArrow;
        end;

      abuTextIconHorz:
        begin
          if FKindVertical then
            btn.Height:= Max(btn.GetTextSize(btn.Caption).cy, FImages.Height)+2*cATButtonIndentArrow
          else
            btn.Width:= btn.GetTextSize(btn.Caption).cx+FImages.Width+3*cATButtonIndentArrow;
        end;
    end;
  end;

  //place controls left to right
  Controls[0].Left:= 0;
  Controls[0].Top:= 0;
  for i:= ControlCount-1 downto 1 do
  begin
    if FKindVertical then
      akind:= akTop
    else
      akind:= akLeft;
    Controls[i].AnchorToNeighbour(akind, 0, Controls[i-1]);
  end;

  //paint
  for i:= 0 to ControlCount-1 do
    Controls[i].Invalidate;
end;

function TATButtonsToolbar.ButtonCount: integer;
begin
  Result:= ControlCount;
end;

function TATButtonsToolbar.GetButton(AIndex: integer): TATButton;
begin
  Result:= nil;
  if (AIndex>=0) and (AIndex<ControlCount) then
    if Controls[AIndex] is TATButton then
      Result:= Controls[AIndex] as TATButton;
end;

procedure TATButtonsToolbar.AddButton(AImageIndex: integer;
  AOnClick: TNotifyEvent; const ACaption, AHint, ADataString: string;
  AShowCaption: boolean);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
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
    if FKindVertical then
      b.Kind:= abuTextIconVert
    else
      b.Kind:= abuTextIconHorz;
  end;
end;

procedure TATButtonsToolbar.AddDropdown(
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
  b.Caption:= ACaption;
  b.Hint:= AHint;
  b.DataString:= ADataString;
  b.ShowHint:= true;
  b.Flat:= true;
  b.PopupMenu:= AMenu;

  if ADropdownEvent=nil then
    b.OnClick:= @PopupForDropdownClick
  else
    b.OnClick:= ADropdownEvent;

  if ACaption<>'' then
    b.Kind:= abuTextArrow
  else
    b.Kind:= abuArrowOnly;
end;

procedure TATButtonsToolbar.AddSep;
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Caption:= '';
  b.Flat:= true;
  if FKindVertical then
    b.Kind:= abuSeparatorVert
  else
    b.Kind:= abuSeparatorHorz;
  b.Enabled:= false;
end;

procedure TATButtonsToolbar.PopupForDropdownClick(Sender: TObject);
var
  C: TControl;
  P: TPoint;
begin
  C:= Sender as TControl;
  P:= C.ClientToScreen(Point(0, C.Height));
  C.PopupMenu.PopUp(P.X, P.Y);
end;


end.

