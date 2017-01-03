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

  TATButtonsToolbar = class(TPanel)
  private
    FImages: TImageList;
    FStringSep: string;
    procedure PopupForDropdownClick(Sender: TObject);
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
    function GetButton(AIndex: integer): TATButton;
    property Buttons[AIndex: integer]: TATButton read GetButton;
    property StringSeparator: string read FStringSep write FStringSep;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property Images: TImageList read FImages write FImages;
  end;

implementation

{ TATButtonsToolbar }

constructor TATButtonsToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '';
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  FImages:= nil;
  FStringSep:= Utf8Encode(#$25be);
end;

destructor TATButtonsToolbar.Destroy;
begin
  inherited;
end;

procedure TATButtonsToolbar.UpdateControls;
var
  btn: TATButton;
  i: integer;
begin
  if not Assigned(FImages) then exit;
  if ControlCount=0 then exit;

  Height:= FImages.Height+2*cATButtonIndent;

  for i:= ControlCount-1 downto 0 do
  begin
    btn:= Controls[i] as TATButton;
    btn.Height:= Self.Height;

    case btn.Kind of
      abuDropdown:
        btn.Width:=
          cATButtonArrowSize+
          2*cATButtonIndentArrow+
          IfThen(btn.Caption<>'', btn.GetTextWidth(btn.Caption)+cATButtonIndent);
      abuSeparator:
        btn.Width:= 2*cATButtonIndentArrow
      else
        btn.Width:=
          2*cATButtonIndent+
          IfThen(btn.ShowCaption, btn.GetTextWidth(btn.Caption))+
          IfThen((btn.ImageIndex>=0), FImages.Width)+
          IfThen((btn.ImageIndex>=0) and (btn.Caption<>''), cATButtonIndent);
      end;
  end;

  //place controls left to right
  Controls[0].Left:= 0;
  Controls[0].Top:= 0;
  for i:= ControlCount-1 downto 1 do
    Controls[i].AnchorToNeighbour(akLeft, 0, Controls[i-1]);

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

procedure TATButtonsToolbar.AddButton(
  AImageIndex: integer;
  AOnClick: TNotifyEvent;
  const ACaption, AHint, ADataString: string;
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
  b.ShowCaption:= AShowCaption;
  b.ShowHint:= true;
  b.OnClick:= AOnClick;
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
  b.Kind:= abuDropdown;
  b.PopupMenu:= AMenu;
  if ADropdownEvent=nil then
    b.OnClick:= @PopupForDropdownClick
  else
    b.OnClick:= ADropdownEvent;
end;

procedure TATButtonsToolbar.AddSep;
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Caption:= '';
  b.Flat:= true;
  b.Kind:= abuSeparator;
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

