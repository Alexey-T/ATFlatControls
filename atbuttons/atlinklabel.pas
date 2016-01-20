unit ATLinkLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls,
  LclIntf;

type
  { TLinkLabel }

  TLinkLabel = class(TLabel)
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

constructor TLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  Cursor:= crHandPoint;
  ShowHint:= true;
  ColorLinkNormal:= clBlue;
  ColorLinkMouseover:= clRed;
  Font.Color:= ColorLinkNormal;
end;

procedure TLinkLabel.SetLink(AValue: string);
begin
  if FLink=AValue then Exit;
  FLink:= AValue;
  Hint:= AValue;
end;

procedure TLinkLabel.Click;
begin
  if Link<>'' then
    OpenURL(Link);
end;

procedure TLinkLabel.MouseEnter;
begin
  Font.Color:= ColorLinkMouseover;
  Font.Style:= Font.Style+[fsUnderline];
end;

procedure TLinkLabel.MouseLeave;
begin
  Font.Color:= ColorLinkNormal;
  Font.Style:= Font.Style-[fsUnderline];
end;

end.

