unit DemoForm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ATStatusBar,
  ATFlatThemes;

type

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bDel: TButton;
    ColorButton1: TColorButton;
    Edit1: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackScale: TTrackBar;
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure TrackScaleChange(Sender: TObject);
  private
    { Private declarations }
    procedure StatusClick(Sender: TObject; AIndex: Integer);
    procedure StatusDraw(Sender: TObject; AIndex: Integer;
      ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t, t0, t1: TATStatus;
  end;

var
  Form1: TForm1;

implementation

uses StrUtils, Math;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  D: TATStatusData;
begin
  t:= TATStatus.Create(Self);
  t.Parent:= Self;
  t.Images:= ImageList1;
  t.OnPanelClick:= StatusClick;
  t.OnPanelDrawAfter:= StatusDraw;

  t.AddPanel(-1, 100, taLeftJustify, 'Left', 0);//, 0, true);
  t.AddPanel(-1, 100, taCenter, 'Center', 1);//, 0, true);
  t.AddPanel(-1, 150, taLeftJustify, 'Long wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');//, -1, 0, true);
  t.AddPanel(-1, 100, taRightJustify, 'Right', 0);//, 0, true);
  t.AddPanel(-1, 80, taLeftJustify, '', 0);
  t.AddPanel(-1, 80, taCenter, '', 0);
  t.AddPanel(-1, 80, taRightJustify, '', 0);

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
  t0.Height:= 32;
  t0.HeightInitial:= 32;

  t0.AddPanel(-1, 50, taRightJustify, 'Rt', -1, 0, false, false, clNavy);
  t0.AddPanel(-1, 50, taRightJustify, 'Rt2', -1, 0, false, false, clGreen);
  t0.AddPanel(-1, 300, taLeftJustify, 'Long wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww', -1, 0, false, true);
  t0.AddPanel(-1, 100, taCenter, 'Center');

  //-----------------------------------
  t1:= TATStatus.Create(Self);
  t1.Parent:= Self;
  t1.Top:= t0.Top-2;

  t1.Color:= clLtGray;
  t1.ColorBorderTop:= clWhite;
  t1.ColorBorderR:= clGray;
  t1.ColorBorderD:= clGray;
  t1.ColorBorderL:= clWhite;
  t1.ColorBorderU:= clNone;
  t1.Height:= 35;
  t1.HeightInitial:= 35;

  t1.AddPanel(-1, 20, taLeftJustify, 'Auto-sized', -1, 0, true, false);
  t1.AddPanel(-1, 20, taRightJustify, 'Also auto-sized', -1, 0, true, false);

  D:= t1.GetPanelData(0);
  D.FontSize:= 14;
  D:= t1.GetPanelData(1);
  D.FontSize:= 19;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
var
  D: TATStatusData;
begin
  if t.PanelCount=0 then exit;
  D:= t.GetPanelData(t.PanelCount-1);
  if D=nil then exit;
  D.ColorLine:= ColorButton1.ButtonColor;
  D.ColorLine2:= ColorButton1.ButtonColor;
  t.Invalidate;
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  t.AddPanel(-1, 50, taCenter, 'n'+StringOfChar('n', Random(8)));
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

procedure TForm1.TrackScaleChange(Sender: TObject);
begin
  ATFlatTheme.ScalePercents:= TrackScale.Position;
  ATFlatTheme.ScaleFontPercents:= TrackScale.Position;
  t.Invalidate;
  t0.Invalidate;
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
