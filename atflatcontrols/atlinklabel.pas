{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATLinkLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls,
  LclIntf;

type
  { TATLabelLink }

  TATLabelLink = class(TLabel)
  private
    FLink: string;
    FColorLinkNormal: TColor;
    FColorLinkMouseover: TColor;
    procedure SetLink(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  published
    property Link: string read FLink write SetLink;
    property ColorLinkNormal: TColor read FColorLinkNormal write FColorLinkNormal;
    property ColorLinkMouseover: TColor read FColorLinkMouseover write FColorLinkMouseover;
  end;

implementation

constructor TATLabelLink.Create(AOwner: TComponent);
begin
  inherited;
  Cursor:= crHandPoint;
  ShowHint:= true;
  ColorLinkNormal:= clBlue;
  ColorLinkMouseover:= clRed;
  Font.Color:= ColorLinkNormal;
end;

procedure TATLabelLink.SetLink(AValue: string);
begin
  if FLink=AValue then Exit;
  FLink:= AValue;
  Hint:= AValue;
end;

procedure TATLabelLink.Click;
begin
  if Link<>'' then
    OpenURL(Link);
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TATLabelLink.MouseEnter;
begin
  Font.Color:= ColorLinkMouseover;
  Font.Style:= Font.Style+[fsUnderline];
end;

procedure TATLabelLink.MouseLeave;
begin
  Font.Color:= ColorLinkNormal;
  Font.Style:= Font.Style-[fsUnderline];
end;

end.

