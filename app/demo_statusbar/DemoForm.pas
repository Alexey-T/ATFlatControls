unit DemoForm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATStatusBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bDel: TButton;
    Edit1: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    procedure StatusClick(Sender: TObject; AIndex: Integer);
    procedure StatusDraw(Sender: TObject; AIndex: Integer;
      ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t, t0: TATStatus;
  end;

var
  Form1: TForm1;

implementation

uses StrUtils, Math;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  t:= TATStatus.Create(Self);
  t.Parent:= Self;
  t.Images:= ImageList1;
  t.OnPanelClick:= StatusClick;
  t.OnPanelDrawAfter:= StatusDraw;

  t.AddPanel(100, taLeftJustify, 'Left', 0);
  t.AddPanel(100, taCenter, 'Center', 1);
  t.AddPanel(150, taLeftJustify, 'Long wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');
  t.AddPanel(100, taRightJustify, 'Right', 0);
  t.AddPanel(80, taLeftJustify, '', 0);
  t.AddPanel(80, taCenter, '', 0);
  t.AddPanel(80, taRightJustify, '', 0);

  //-----------------------------------
  t0:= TATStatus.Create(Self);
  t0.Parent:= Self;
  t0.Images:= ImageList1;
  t0.OnPanelClick:= StatusClick;
  t0.Top:= ClientHeight-2;

  t0.Color:= clLtGray;
  t0.ColorBorderTop:= clWhite;
  t0.ColorBorderR:= clGray;
  t0.ColorBorderD:= clGray;
  t0.ColorBorderL:= clWhite;
  t0.ColorBorderU:= clNone;
  t0.Font.Color:= clNavy;
  t0.Font.Size:= 13;
  t0.Height:= 32;

  t0.AddPanel(50, taRightJustify, 'Rt');
  t0.AddPanel(50, taRightJustify, 'Rt2');
  t0.AddPanel(100, taCenter, 'Center');
  t0.AddPanel(300, taLeftJustify, 'Long wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  t.AddPanel(50, taCenter, 'n'+StringOfChar('n', Random(8)));
end;

procedure TForm1.bDelClick(Sender: TObject);
begin
  if t.PanelCount>0 then
    t.DeletePanel(t.PanelCount-1);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  d: TATStatusData;
begin
  if t.PanelCount=0 then Exit;
  d:= t.GetPanelData(t.PanelCount-1);
  d.Caption:= Edit1.Text;
  t.Invalidate;
end;

procedure TForm1.StatusClick(Sender: TObject; AIndex: Integer);
var
  D: TATStatusData;
begin
  Label1.Caption:= 'Clicked panel '+IntToStr(AIndex) + ' of '+
    IfThen(Sender=t, 'top bar', 'bottom bar');

  D:= (Sender as TATStatus).GetPanelData(AIndex);
  if D=nil then exit;
  if D.ColorFont=clNone then
  begin
    D.ColorFont:= clGreen;
    D.ColorBack:= clYellow;
  end
  else
  begin
    D.ColorFont:= clNone;
    D.ColorBack:= clNone;
  end;
  (Sender as TATStatus).Invalidate;
end;

procedure TForm1.StatusDraw(Sender: TObject; AIndex: Integer;
  ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  Exit;
  //test
  ACanvas.Pen.Color:= clRed;
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Left+10, ARect.Top+20);
  ACanDraw:= false;
end;

end.
