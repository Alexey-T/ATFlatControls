{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATLinkLabel;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ShellAPI, Windows, Messages;
  {$ifdef FPC}
  LclIntf;
  {$endif}

type
  { TATLabelLink }

  TATLabelLink = class(TLabel)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;

    FLink: string;
    FColorLinkNormal: TColor;
    FColorLinkMouseover: TColor;
    procedure SetLink(AValue: string);
    procedure CMMouseEnter(var msg: TMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage);
      message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Click; override;
    {$ifdef FPC}
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    {$endif}
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

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
    {$ifdef FPC}
    OpenURL(Link);
    {$endif}
    ShellExecuteW(0, nil, PWideChar(Link), nil, nil, SW_SHOWNORMAL);
  if Assigned(OnClick) then
    OnClick(Self);
end;

{$ifdef FPC}
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
{$endif}

procedure TATLabelLink.CMMouseEnter(var msg: TMessage);
begin
  DoMouseEnter;
end;

procedure TATLabelLink.CMMouseLeave(var msg: TMessage);
begin
  DoMouseLeave;
end;

procedure TATLabelLink.DoMouseEnter;
begin
  Invalidate;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TATLabelLink.DoMouseLeave;
begin
  Invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

end.

