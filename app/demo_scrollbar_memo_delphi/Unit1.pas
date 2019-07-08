unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  ATScrollBar, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Math,
  Vcl.AppEvnts, System.UITypes;

type
  TMemo  = class(Vcl.StdCtrls.TMemo)
  private
   procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
   procedure WMVScroll(var Msg: TWMHScroll); message WM_VSCROLL;
   procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
   procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    ATVScrollbar: TATScrollbar;
    PanelRight: TPanel;
    cbWordWrap: TCheckBox;
    ATHScrollbar: TATScrollbar;
    PanelMemo: TPanel;
    ApplicationEvents1: TApplicationEvents;
    Button1: TButton;
    FontDialog1: TFontDialog;
    RadioGroup1: TRadioGroup;
    cbDark: TCheckBox;
    procedure ATVScrollbarChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cbWordWrapClick(Sender: TObject);
    procedure ATHScrollbarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure cbDarkClick(Sender: TObject);
  private
    procedure UpdateScrollbar;
    procedure SetScrollWidth(EndSpace:integer=15);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
const
  CustomSB = 0;
  SystemSB = 1;
  BothSB = 2;

implementation

uses ShellAPI;

{$R *.dfm}

{TMEMO FUNCTIONS}

function LineHeight(Memo: TObject): Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  TextMetric: TTextMetric;
  EditRect: TRect;
begin

  Result := 0;

  if Memo is TMemo then
  with Memo as TMemo do
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TextMetric);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);

    Perform(EM_GETRECT, 0, LPARAM(@EditRect));
    Result := TextMetric.tmHeight;
  end;

end;

function GetTextMetric(Memo: TObject): TTextMetric;
var
  DC: HDC;
  SaveFont: HFONT;
  TextMetric: TTextMetric;
  EditRect: TRect;
begin

  //Result := nil;

  if Memo is TMemo then
  with Memo as TMemo do
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TextMetric);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);

    Perform(EM_GETRECT, 0, LPARAM(@EditRect));
    Result := TextMetric;
  end;

end;

function GetVisibleLineCount(Memo: TObject): Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  TextMetric: TTextMetric;
  EditRect: TRect;
begin

  Result := 0;

  if Memo is TMemo then
  with Memo as TMemo do
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);

    GetTextMetrics(DC, TextMetric);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);

    Perform(EM_GETRECT, 0, LPARAM(@EditRect));

    Result := (EditRect.Bottom - EditRect.Top) div TextMetric.tmHeight;

  end;

end;

function ContentRect(Memo: TObject): TRect;
var
  EditRect: TRect;
  Canvas: TControlCanvas;
  i: integer;
  S: string;
Begin
  Canvas:= TControlCanvas.Create;
  try
    i := 0;
    if Memo is TRichEdit then
    begin
      S := TRichEdit(Memo).Text;
      Canvas.Control := TRichEdit(Memo);
      Canvas.Font := TRichEdit(Memo).Font;
      if TRichEdit(Memo).WordWrap then i := DT_WORDBREAK;
      TRichEdit(Memo).Perform(EM_GETRECT, 0, LPARAM(@EditRect));
    end;
    if Memo is TMemo then
    begin
      S := TMemo(Memo).Text;
      Canvas.Control := TMemo(Memo);
      Canvas.Font := TMemo(Memo).Font;
      if TMemo(Memo).WordWrap then i := DT_WORDBREAK;
      TMemo(Memo).Perform(EM_GETRECT, 0, LPARAM(@EditRect));
    end;

    DrawText(Canvas.Handle,
      PChar(S),//Memo.Text),
      Length(S),//(Memo.Text),
      EditRect,
      DT_LEFT or i or DT_CALCRECT);

    Result := EditRect;

  finally
    Canvas.Free;
  end;
end;

procedure TMemo.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  //DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  I: Integer;                 // loops thru all dropped files
  DropPoint: TPoint;          // point where files dropped
begin
  inherited;
  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    // Get count of files dropped
    //DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    for I := 0 to 0 do//Pred(DroppedFileCount) do
    begin
      // get length of file name
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      // create string large enough to store file
      // (Delphi allows for #0 terminating character automatically)
      SetLength(FileName, FileNameLength);
      // get the file name
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      // process file name (application specific)
      // ... processing code here
      //ShowMessage(FileName);
      Clear;
      Form1.ATVScrollbar.Position := 0;
      Form1.ATHScrollbar.Position := 0;
      Form1.ATHScrollbar.Max := 0;
      Lines.LoadFromFile(FileName);
      Form1.UpdateScrollbar;
      SetFocus;
    end;
    // Optional: Get point at which files were dropped
    DragQueryPoint(DropH, DropPoint);
    // ... do something with drop point here
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
  Form1.Caption := 'ATScrollBar Memo Demo - ' + ExtractFileName(FileName);
  Form1.PanelMemo.Invalidate;

end;

procedure TMemo.CNCommand(var Message: TWMCommand);
var
  FirstVisibleLine, SBHPos: integer;
const
  EndSpace = 12; //slight space for breathing room at end of scroll
begin

   if Form1.RadioGroup1.ItemIndex = SystemSB then //custom SBs not used
     exit;

   //These messages let the memo scroll the custom scrollbar(s):

   case Message.NotifyCode of
    EN_VSCROLL :
    begin
      //OutputDebugString('EN_VSCROLL')
      FirstVisibleLine := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);

      if assigned(Form1.ATVScrollbar) then
      if Form1.ATVScrollbar.Position <> FirstVisibleLine then
        Form1.ATVScrollbar.Position := FirstVisibleLine;
    end;
    EN_HSCROLL :
    begin
      //OutputDebugString('EN_HSCROLL')
      SBHPos := Min(
        (((CaretPos.X) * GetTextMetric(Self).tmAveCharWidth) -
          Form1.ATHScrollbar.PageSize) + EndSpace,
            Form1.ATHScrollbar.Max -
              Form1.ATHScrollbar.PageSize + EndSpace);

      Form1.ATHScrollbar.Position := SBHPos;

      Invalidate;
    end;
   end;

   inherited;

end;

procedure TMemo.WMHScroll(var Msg: TWMHScroll);
var
  SInfo: TScrollInfo;
begin
  //OutputDebugString('WM_HSCROLL') ;
  inherited;

  //Note: this is used in the demo to sync AT SB pos with system SB. Probably
  //would not be used in a normal app but useful here:
  if ScrollBars <> ssNone then
  begin
    SInfo.cbSize := SizeOf(SInfo);
    SInfo.fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_HORZ, SInfo);
    Form1.ATHScrollbar.Position := SInfo.nPos ;
    exit;
  end;

end;

procedure TMemo.WMVScroll(var Msg: TWMHScroll);
var
  SInfo: TScrollInfo;
begin
  //OutputDebugString('WM_HSCROLL') ;
  inherited;

  //Note: this is used in the demo to sync AT SB pos with system SB. Probably
  //would not be used in a normal app but useful here:
  if ScrollBars <> ssNone then
  begin
    SInfo.cbSize := SizeOf(SInfo);
    SInfo.fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_VERT, SInfo);
    Form1.ATVScrollbar.Position := SInfo.nPos ;
    exit;
  end;

end;

{MAIN FORM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FontDialog1.Font := Memo1.Font;
  if FontDialog1.Execute then
  begin
    Memo1.Font := FontDialog1.Font;
  end;
  UpdateScrollbar;
end;

procedure TForm1.SetScrollWidth(EndSpace:integer);
var
  i, startLine, endLine, endPos: integer;
begin

    if not Memo1.WordWrap then
    begin
      startLine := Memo1.Perform( EM_GETFIRSTVISIBLELINE,0,0 );
      endLine := startLine + GetVisibleLineCount(Memo1);
      for i := startLine to endLine do
      begin
        endPos :=
          (Length(Memo1.Lines[i]) * GetTextMetric(Memo1).tmAveCharWidth) +
            (EndSpace * GetTextMetric(Memo1).tmAveCharWidth);
        if ATHScrollbar.Max < endPos then
          ATHScrollbar.Max := endPos;
      end;
      ATHScrollbar.Visible :=
        (Memo1.ClientWidth < ATHScrollbar.Max) and
          (RadioGroup1.ItemIndex <> SystemSB)
    end;

end;

procedure TForm1.UpdateScrollbar;
var
  VisibleLineCount: integer;
  NeedVert, NeedHorz: boolean;
  SS: TScrollStyle;
begin

  VisibleLineCount := GetVisibleLineCount(Memo1);

  NeedVert := (Memo1.Lines.Count > VisibleLineCount);
  NeedHorz :=
    (not Memo1.WordWrap) and
      (Memo1.ClientWidth < ATHScrollbar.Max); //ContentRect(Memo1).Width);

  SS := ssNone;

  if NeedVert then
    SS := ssVertical;
  if NeedHorz then
    SS := ssHorizontal;
  if NeedVert and NeedHorz then
    SS := ssBoth;

  {TMEMO}

  // SYSTEM ///////
    case RadioGroup1.ItemIndex of

      CustomSB: begin // Custom Only
        Memo1.ScrollBars := ssNone;
      end;

      SystemSB: begin // System Only
        Memo1.ScrollBars := SS;
      end;

      BothSB: begin // System and Custom
        Memo1.ScrollBars := SS;
      end;

    end;

  // END SYSTEM ///////

  // VERTICAL CUSTOM ///////

    ATVScrollbar.Visible :=
      NeedVert and (RadioGroup1.ItemIndex <> SystemSB);

    if ATVScrollbar.Visible then
      ATHScrollbar.Margins.Right := ATVScrollbar.Width
    else
      ATHScrollbar.Margins.Right := 0;

    ATVScrollbar.Min:= 0;

    ATVScrollbar.LargeChange := 3;
    ATVScrollbar.PageSize := GetVisibleLineCount(Memo1);
    ATVScrollbar.Max := Memo1.Lines.Count + 1;

  // END VERTICAL CUSTOM ///////

  // HORIZONTAL CUSTOM ///////
    ATHScrollbar.Visible :=
      NeedHorz and (RadioGroup1.ItemIndex <> SystemSB);

    ATHScrollbar.Min:= 0;
    //Minimum scroll arrow movement:
    ATHScrollbar.SmallChange := GetTextMetric(Memo1).tmMaxCharWidth;
    ATHScrollbar.LargeChange := ATHScrollbar.SmallChange * 3;

    ATHScrollbar.PageSize := Memo1.ClientWidth;
    //ATHScrollbar.Max := ContentRect(Memo1).Width + EndSpace;

    SetScrollWidth;

  // END HORIZONTAL CUSTOM ///////

  ATVScrollbar.Invalidate;
  ATHScrollbar.Invalidate;

  //Wordwrap change apparently recreates window so we have to reset this here
  //or file drops stop working:
  DragAcceptFiles( Memo1.Handle, True );

end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var
  FirstVisibleLine: integer;
begin

  exit; //debug

  if GetAsyncKeyState(VK_LBUTTON) > 0 then
  begin
    exit;
  end;

  FirstVisibleLine := Memo1.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);

  if ATVScrollbar.Position <> FirstVisibleLine then
    ATVScrollbar.Position := FirstVisibleLine;

end;

procedure TForm1.cbWordWrapClick(Sender: TObject);
begin

  Memo1.WordWrap := cbWordWrap.Checked;
  //if Memo1.WordWrap then Memo1.ScrollBars := ssVertical
  //else Memo1.ScrollBars := ssBoth;

  Memo1.SetFocus;
  UpdateScrollbar;

end;

procedure TForm1.cbDarkClick(Sender: TObject);
begin

  if cbDark.Checked then
  begin
    Memo1.Color := $00323232;
    Memo1.Font.Color := clWhite;
    PanelRight.Color := clGray;
    ATScrollbarTheme.ColorBG := $006c7073;
    ATScrollbarTheme.ColorThumbFill := clGray;
    ATScrollbarTheme.ColorThumbBorder := clWhite;
    ATScrollbarTheme.ColorThumbFillOver := $000066ca;
    ATScrollbarTheme.ColorThumbFillPressed := clSilver;
    ATScrollbarTheme.ColorArrowFillOver:= $000066ca;
  end
  else
  begin
    Memo1.Color := clWindow;
    Memo1.Font.Color := clWindowText;
    PanelRight.Color := Memo1.Color;
    ATScrollbarTheme.ColorBG := $d0d0d0;
    ATScrollbarTheme.ColorThumbFill := $c0c0c0;
    ATScrollbarTheme.ColorThumbBorder := $808080;
    ATScrollbarTheme.ColorThumbFillOver := $d0d0d0;
    ATScrollbarTheme.ColorThumbFillPressed := $e0c0c0;
    ATScrollbarTheme.ColorArrowFillOver:= $d0d0d0;
  end;

  ATVScrollbar.Invalidate;
  ATHScrollbar.Invalidate;

  PanelMemo.Color := Memo1.Color;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  aRect: TRect;
begin

  DragAcceptFiles( Memo1.Handle, True );

  ATScrollbarTheme.ThumbMinSize := GetSystemMetrics( SM_CXHTHUMB );

  aRect := Rect(0,0,Memo1.ClientHeight, Memo1.ClientWidth);
  Memo1.Perform(EM_SETRECT,0,Longint(@aRect));

  Memo1.Clear;
  Memo1.Lines.Add('Memo1: Drag and drop text files to open in the memo.');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Note: Every component is different, so what works here for ' +
                  'a TMemo may not work for a different component.');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Drag and Drop a text file on this TMemo to open.');

end;

type
  TControlHack = class(TControl);
procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
  c: TControlHack;
begin

  for i:=0 to ComponentCount-1 do
    if Components[i] is TControl then begin
      c:=TControlHack(Components[i]);
      if PtInRect(c.ClientRect,c.ScreenToClient(MousePos)) then
      begin
        Handled:=c.DoMouseWheel(shift,WheelDelta,MousePos);

        if (c = TControlHack(Memo1)) or (c = TControlHack(ATVScrollbar)) then
        begin
          if Memo1.CanFocus and Memo1.Showing then Memo1.SetFocus;
          if not Memo1.Focused then exit;
          if WheelDelta < 120 then //in [scPageDown, scLineDown] then
          begin
            //Memo1.Perform(EM_SCROLL,SB_LINEDOWN,0) //one line down
            Memo1.Perform(EM_LineSCROLL,0, 1 );
            //ATVScrollbar.Position := ATVScrollbar.Position + 3;
          end
          else //if ScrollCode in [scPageUp, scLineUp] then
          begin
            //Memo1.Perform(EM_SCROLL,SB_LINEUP,0); //one line up
            Memo1.Perform(EM_LineSCROLL,0, -1 );
            //ATVScrollbar.Position := ATVScrollbar.Position - 3;
          end;
        end;
      end;
   end;

  //if ATVScrollbar.Position <> (Memo1.Perform(EM_GETFIRSTVISIBLELINE, 0, 0)) then
  //  ATVScrollbar.Position := Memo1.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);

end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateScrollbar;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  UpdateScrollbar;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin

  UpdateScrollbar;

end;

procedure TForm1.ATVScrollbarChange(Sender: TObject);
begin

  SetScrollWidth;
  SendMessage(Memo1.Handle, WM_VSCROLL ,MAKEWPARAM( SB_THUMBTRACK,
    ATVScrollbar.Position) ,0);

end;

procedure TForm1.ATHScrollbarChange(Sender: TObject);
begin

  SendMessage(Memo1.Handle, WM_HSCROLL,MAKEWPARAM( SB_THUMBTRACK,
    ATHScrollbar.Position ) ,0);

end;


end.
