unit DemoForm;

interface

uses
  {$ifndef fpc}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATScrollBar, ComCtrls;

type
  TFormDemo = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    chkDraw: TCheckBox;
    trackBor: TTrackBar;
    Label1: TLabel;
    labv: TLabel;
    labh: TLabel;
    Label2: TLabel;
    trackPage: TTrackBar;
    Label3: TLabel;
    trackSize: TTrackBar;
    Label4: TLabel;
    trackLonger: TTrackBar;
    Label5: TLabel;
    trackCornerV: TTrackBar;
    trackCornerH: TTrackBar;
    ListArrows: TListBox;
    chkInstant: TCheckBox;
    chkDecorDbl1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkDrawClick(Sender: TObject);
    procedure trackBorChange(Sender: TObject);
    procedure trackPageChange(Sender: TObject);
    procedure trackSizeChange(Sender: TObject);
    procedure trackLongerChange(Sender: TObject);
    procedure trackCornerVChange(Sender: TObject);
    procedure trackCornerHChange(Sender: TObject);
    procedure ListArrowsClick(Sender: TObject);
    procedure chkDecorDbl1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeH(S: TObject);
    procedure ChangeV(S: TObject);
    procedure DrawEvent(S: TObject; AType: TATScrollbarElemType;
      ACanvas: TCanvas; const ARect: TRect; var ACanDo: boolean);
  public
    { Public declarations }
    bar_h, bar_v, bar_h1, bar_v1: TATScrollbar;
  end;

var
  FormDemo: TFormDemo;

implementation

uses StrUtils, Math;

{$R *.dfm}

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  bar_h:= TATScrollbar.Create(Self);
  bar_h.Parent:= Panel1;
  bar_h.Align:= alBottom;
  bar_h.Kind:= sbHorizontal;
  bar_h.OnChange:= ChangeH;
  bar_h.Min:= 20;
  bar_h.Max:= 200;

  //-----------------------------------
  bar_v:= TATScrollbar.Create(Self);
  bar_v.Parent:= Panel1;
  bar_v.Align:= alRight;
  bar_v.Kind:= sbVertical;
  bar_v.OnChange:= ChangeV;
  bar_v.Min:= 10;
  bar_v.Max:= 100;

  bar_v.Width:= ATScrollbarTheme.InitialSize;
  bar_h.Height:= ATScrollbarTheme.InitialSize;
  bar_h.IndentCorner:= 100;

  //--------------
  bar_h1:= TATScrollbar.Create(Self);
  bar_h1.Parent:= Panel2;
  bar_h1.Height:= ATScrollbarTheme.InitialSize;
  bar_h1.Align:= alBottom;
  bar_h1.Kind:= sbHorizontal;
  bar_h1.IndentCorner:= -100; //negative

  bar_v1:= TATScrollbar.Create(Self);
  bar_v1.Parent:= Panel2;
  bar_v1.Width:= ATScrollbarTheme.InitialSize;
  bar_v1.Align:= alLeft;
  bar_v1.Kind:= sbVertical;
end;

procedure TFormDemo.ListArrowsClick(Sender: TObject);
begin
  if ListArrows.ItemIndex<=Ord(High(TATScrollbarArrowsStyle)) then
  begin
    ATScrollbarTheme.ArrowStyleH:= TATScrollbarArrowsStyle(ListArrows.ItemIndex);
    ATScrollbarTheme.ArrowStyleV:= ATScrollbarTheme.ArrowStyleH;
  end
  else
  begin
    ATScrollbarTheme.ArrowStyleH:= asaArrowsAbove;
    ATScrollbarTheme.ArrowStyleV:= asaArrowsBelow;
  end;

  bar_v.Invalidate;
  bar_h.Invalidate;
  bar_v1.Invalidate;
  bar_h1.Invalidate;

end;

procedure TFormDemo.DrawEvent(S: TObject; AType: TATScrollbarElemType;
  ACanvas: TCanvas; const ARect: TRect; var ACanDo: boolean);
const
  cc: array[TATScrollbarElemType] of TColor = (
    clYellow, clYellow,
    clCream, clCream,
    $30a030, $00e000, 
    $e0a0c0, clNavy,
    clRed div 2, clRed,
    $e05050);
var
  str: string;
  p: TPoint;
begin
  ACanvas.Brush.Color:= cc[AType];
  ACanvas.FillRect(ARect);
  ACanDo:= false;

  case AType of
    aseArrowUp: str:= '^';
    aseArrowDown: str:= 'v';
    aseArrowLeft: str:= '<';
    aseArrowRight: str:= '>';
    aseScrollThumbH: str:= '==';
    aseScrollThumbV: str:= '||';
    else str:= '';
  end;

  if str<>'' then
  begin
    p.x:= (ARect.Left+ARect.Right-ACanvas.TextWidth(str)) div 2;
    p.y:= (ARect.Top+ARect.Bottom-ACanvas.TextHeight(str)) div 2;
    ACanvas.TextOut(p.x, p.y, str);
  end;
end;

procedure TFormDemo.chkDecorDbl1Click(Sender: TObject);
begin
  ATScrollbarTheme.ThumbMarkerDecorDouble := chkDecorDbl1.Checked;
  bar_v.Invalidate;
  bar_h.Invalidate;
  bar_v1.Invalidate;
  bar_h1.Invalidate;
end;

procedure TFormDemo.chkDrawClick(Sender: TObject);
begin
  if chkDraw.Checked then
  begin
    bar_h.OnOwnerDraw:= DrawEvent;
    bar_v.OnOwnerDraw:= DrawEvent;
  end
  else
  begin
    bar_h.OnOwnerDraw:= nil;
    bar_v.OnOwnerDraw:= nil;
  end;
  bar_h.Invalidate;
  bar_v.Invalidate;
end;

procedure TFormDemo.trackBorChange(Sender: TObject);
begin
  ATScrollbarTheme.BorderSize:= trackBor.Position;
  bar_v.Invalidate;
  bar_h.Invalidate;
  bar_v1.Invalidate;
  bar_h1.Invalidate;
end;

procedure TFormDemo.ChangeH(S: TObject);
begin
  labh.Caption:= Format('Horz %d (%d .. %d)', [bar_h.Position, bar_h.Min, bar_h.Max]);
end;

procedure TFormDemo.ChangeV(S: TObject);
begin
  labv.Caption:= Format('Vert %d (%d .. %d)', [bar_v.Position, bar_v.Min, bar_v.Max]);
end;

procedure TFormDemo.trackPageChange(Sender: TObject);
begin
  bar_v.PageSize:= trackPage.Position;
  bar_h.PageSize:= bar_v.PageSize;
end;

procedure TFormDemo.trackSizeChange(Sender: TObject);
var
  n: Integer;
begin
  n:= trackSize.Position;
  ATScrollbarTheme.ScalePercents:= n;
  bar_v.Update;
  bar_h.Update;
  bar_v1.Update;
  bar_h1.Update;
end;

procedure TFormDemo.trackLongerChange(Sender: TObject);
begin
  ATScrollbarTheme.ArrowLengthPercents:= trackLonger.Position;
  bar_h.Invalidate;
  bar_v.Invalidate;
  bar_h1.Invalidate;
  bar_v1.Invalidate;
end;

procedure TFormDemo.trackCornerVChange(Sender: TObject);
begin
  bar_v.IndentCorner:= trackCornerV.Position;
  bar_v.Invalidate;
end;

procedure TFormDemo.trackCornerHChange(Sender: TObject);
begin
  bar_h.IndentCorner:= trackCornerH.Position;
  bar_h.Invalidate;
end;

end.
