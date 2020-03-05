unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, attabs,
  unit_frame1, unit_frame2, unit_frame3;

type

  { TForm1 }

  TForm1 = class(TForm)
    Tabs: TATTabs;
    PanelTabs: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabsTabClick(Sender: TObject);
  private
    f1: TForm;
    f2: TForm;
    f3: TForm;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  d: TATTabData;
begin
  Tabs.ColorTabActive:= clBtnFace;
  Tabs.ColorFontActive:= clBlack;
  Tabs.OptSpaceBetweenTabs:= 8;
  Tabs.OptShowPlusTab:= false;
  Tabs.OptShowXButtons:= atbxShowNone;
  Tabs.OptButtonLayout:= '';

  f1:= TFormFrame1.Create(Self);
  f2:= TFormFrame2.Create(Self);
  f3:= TFormFrame3.Create(Self);

  f1.Parent:= PanelTabs;
  f2.Parent:= PanelTabs;
  f3.Parent:= PanelTabs;

  f1.Align:= alClient;
  f2.Align:= alClient;
  f3.Align:= alClient;

  f1.Hide;
  f2.Hide;
  f3.Hide;

  d:= Tabs.GetTabData(0);
  d.TabObject:= f1;
  d:= Tabs.GetTabData(1);
  d.TabObject:= f2;
  d:= Tabs.GetTabData(2);
  d.TabObject:= f3;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Tabs.TabIndex:= 1;
end;

procedure TForm1.TabsTabClick(Sender: TObject);
var
  d: TATTabData;
begin
  if f1=nil then exit;

  f1.Hide;
  f2.Hide;
  f3.Hide;

  d:= Tabs.GetTabData(Tabs.TabIndex);
  (d.TabObject as TForm).Show;
end;

end.

