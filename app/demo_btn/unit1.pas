unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ATButtons;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkEn: TCheckBox;
    chkFocus: TCheckBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    PanelToolbar: TPanel;
    procedure chkEnChange(Sender: TObject);
    procedure chkFocusChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure BtnToggleClick(Sender: TObject);
    procedure BtnColorsClick(Sender: TObject);
  public
    { public declarations }
    b, b_colors, b2, b3,
    b_tool1, b_tool2, b_tool3: TATButton;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp:= TBitmap.Create;
  bmp.LoadFromFile(ExtractFilepath(Application.Exename)+'bmp1.bmp');

  b:= TATButton.create(self);
  b.parent:= self;
  b.SetBounds(50, 100, 180, 40);
  b.Caption:= 'Toggle checks';
  b.OnClick:= @BtnToggleClick;

  b2:= TATButton.create(self);
  b2.parent:= self;
  b2.SetBounds(50, 150, 80, 30);
  b2.Caption:= 'Check1';
  b2.Checkable:= true;

  b3:= TATButton.create(self);
  b3.parent:= self;
  b3.SetBounds(130, 150, 80, 30);
  b3.Caption:= 'Check2';
  b3.Checkable:= true;

  b_colors:= TATButton.create(self);
  b_colors.parent:= self;
  b_colors.SetBounds(120, chkFocus.Top, 80, 40);
  b_colors.Caption:= '';
  b_colors.Bitmap.Assign(bmp);
  b_colors.OnClick:= @BtnColorsClick;

  b_tool1:= TATButton.create(self);
  b_tool1.parent:= PanelToolbar;
  b_tool1.SetBounds(5, 30, 35, 35);
  b_tool1.Caption:= '';
  b_tool1.ImageList:= ImageList1;
  b_tool1.ImageIndex:= 0;
  b_tool1.Focusable:= false;
  b_tool1.Flat:= true;

  b_tool2:= TATButton.create(self);
  b_tool2.parent:= PanelToolbar;
  b_tool2.SetBounds(40, 30, 35, 35);
  b_tool2.Caption:= '';
  b_tool2.ImageList:= ImageList1;
  b_tool2.ImageIndex:= 1;
  b_tool2.Focusable:= false;
  b_tool2.Flat:= true;

  b_tool3:= TATButton.create(self);
  b_tool3.parent:= PanelToolbar;
  b_tool3.SetBounds(75, 30, 35, 35);
  b_tool3.Caption:= '';
  b_tool3.ImageList:= ImageList1;
  b_tool3.ImageIndex:= 2;
  b_tool3.Focusable:= false;
  b_tool3.Flat:= true;

end;

procedure TfmMain.chkFocusChange(Sender: TObject);
var
  en: boolean;
begin
  en:= chkFocus.Checked;
  b.Focusable:= en;
  b_colors.Focusable:= en;
  b2.Focusable:= en;
  b3.Focusable:= en;
end;

procedure TfmMain.chkEnChange(Sender: TObject);
var
  en: boolean;
begin
  en:= chkEn.Checked;
  b.Enabled:= en;
  b_colors.Enabled:= en;
  b2.Enabled:= en;
  b3.Enabled:= en;
end;

procedure TfmMain.BtnToggleClick(Sender: TObject);
begin
  with b2 do Checked:= not Checked;
  with b3 do Checked:= not Checked;
end;

procedure TfmMain.BtnColorsClick(Sender: TObject);
  function SomeColor: TColor;
  begin
    Result:= $a00000+Random($fffff);
  end;
begin
  with ATButtonTheme do
  begin
    ColorFont:= SomeColor;
    ColorBgPassive:= SomeColor;
    ColorBgOver:= SomeColor;
    ColorBgChecked:= SomeColor;
  end;
  Invalidate;
end;

end.

