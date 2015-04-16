unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ATButtons;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkFocus: TCheckBox;
    procedure chkFocusChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    b, bi, b2, b3: TATSimpleButton;
    procedure FClick(Snd: TObject);
    procedure ImgClick(Snd: TObject);
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

  b:= TATSimpleButton.create(self);
  b.parent:= self;
  b.SetBounds(50, 100, 180, 40);
  b.Caption:= 'Toggle chk';
  b.OnClick:= @FClick;

  bi:= TATSimpleButton.create(self);
  bi.parent:= self;
  bi.SetBounds(350, 100, 80, 40);
  bi.Caption:= '';
  bi.Bitmap:= bmp;
  bi.OnClick:= @ImgClick;

  b2:= TATSimpleButton.create(self);
  b2.parent:= self;
  b2.SetBounds(50, 150, 80, 30);
  b2.Caption:= 'Check1';
  b2.Checkable:= true;

  b3:= TATSimpleButton.create(self);
  b3.parent:= self;
  b3.SetBounds(130, 150, 80, 30);
  b3.Caption:= 'Check2';
  b3.Checkable:= true;
end;

procedure TfmMain.chkFocusChange(Sender: TObject);
var
  en: boolean;
begin
  en:= chkFocus.Checked;
  b.Focusable:= en;
  bi.Focusable:= en;
  b2.Focusable:= en;
  b3.Focusable:= en;
end;

procedure TfmMain.FClick(Snd: TObject);
begin
  with b2 do Checked:= not Checked;
  with b3 do Checked:= not Checked;
end;

procedure TfmMain.ImgClick(Snd: TObject);
begin
  with ATButtonTheme do
  begin
    ColorFont:= $e0e0e0;
    ColorBgPassive:= $00a000;
    ColorBgOver:= $00d000;
    ColorBgChecked:= clYellow;
  end;
  Invalidate;
end;

end.

