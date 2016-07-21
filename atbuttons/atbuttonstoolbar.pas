{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtonsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls,
  ImgList, Menus, ATButtons;

type
  { TATButtonsToolbar }

  TATButtonsToolbar = class(TPanel)
  private
    FImageList: TImageList;
    FSizeIncToIcon: integer;
    FSizeSep: integer;
    procedure PopupForDropdownClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(AImageIndex: integer; AOnClick: TNotifyEvent);
    procedure AddDropdown(AMenu: TPopupMenu);
    procedure AddSep;
    procedure UpdateControls;
    function ButtonCount: integer;
    function GetButton(AIndex: integer): TATButton;
    property Buttons[AIndex: integer]: TATButton read GetButton;
  published
    property Align;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property Images: TImageList read FImageList write FImageList;
    property SizeIncrementToIcon: integer read FSizeIncToIcon write FSizeIncToIcon;
    property SizeSeparator: integer read FSizeSep write FSizeSep;
  end;

implementation

{ TATButtonsToolbar }

constructor TATButtonsToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '';
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  FImageList:= nil;
  FSizeIncToIcon:= 6;
  FSizeSep:= 14;
end;

destructor TATButtonsToolbar.Destroy;
begin
  inherited;
end;

procedure TATButtonsToolbar.UpdateControls;
var
  C: TControl;
  i: integer;
begin
  if not Assigned(FImageList) then exit;
  if ControlCount=0 then exit;

  Height:= FImageList.Height+FSizeIncToIcon;

  //update control sizes
  //width only for buttons
  for i:= ControlCount-1 downto 0 do
  begin
    C:= Controls[i];
    C.Height:= FImageList.Height+FSizeIncToIcon;
    if C is TATButton then
      if (C as TATButton).ImageIndex>=0 then
        C.Width:= FImageList.Width+FSizeIncToIcon;
  end;

  //place controls left to right
  Controls[0].Left:= 0;
  Controls[0].Top:= 0;
  for i:= ControlCount-1 downto 1 do
    Controls[i].AnchorToNeighbour(akLeft, 0, Controls[i-1]);
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

procedure TATButtonsToolbar.AddButton(AImageIndex: integer; AOnClick: TNotifyEvent);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Width:= 20;
  b.Height:= 20;
  b.Caption:= '';
  b.Flat:= true;
  b.Images:= FImageList;
  b.ImageIndex:= AImageIndex;
  b.OnClick:= AOnClick;
end;

procedure TATButtonsToolbar.AddDropdown(AMenu: TPopupMenu);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Width:= FSizeSep;
  b.Height:= 20;
  b.Caption:= Utf8Encode(#$25be);
  b.Flat:= true;
  b.PopupMenu:= AMenu;
  b.OnClick:= @PopupForDropdownClick;
end;

procedure TATButtonsToolbar.AddSep;
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Caption:= '';
  b.Enabled:= false;
  b.Width:= FSizeSep;
  b.Height:= 20;
  b.Flat:= true;
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

