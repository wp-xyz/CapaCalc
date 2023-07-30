unit ccMain;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, FileUtil, Graphics,
  Forms, Controls, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  ccBaseFrame;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnCalculate: TButton;
    BtnClose: TButton;
    LblAccuracy: TLabel;
    PageControl: TPageControl;
    Panel1: TPanel;
    PgPlanar: TTabSheet;
    PgSpherical: TTabSheet;
    PgCylindrical: TTabSheet;
    PgLinePlane: TTabSheet;
    PgCylPlane: TTabSheet;
    Pg2Cyl: TTabSheet;
    PgSeries: TTabSheet;
    PgPNJunction: TTabSheet;
    PgParallel: TTabSheet;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnCalculateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    { private declarations }
    FActivated: Boolean;
    procedure CalcCompleteHandler(Sender: TObject; AMsg: String; isOK: Boolean);
    procedure Calculate;
    function FindFrame(APage: TTabsheet): TBaseFrame;
    procedure ReadFromIni;
    procedure WriteToIni;
  public
    { public declarations }
    procedure BeforeRun;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles,
  ccPlanarCapFrame, ccSphericalCapFrame, ccCylindricalCapFrame,
  ccLinePlaneCapFrame, ccCylPlaneCapFrame, cc2CylCapFrame,
  ccSeriesCapFrame, ccParallelCapFrame, ccPNJunctionCapFrame;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(GetAppConfigFile(false), '.ini');;
  Result := TMemIniFile.Create(fn);
end;

{ TMainForm }

procedure TMainForm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BtnCalculateClick(Sender: TObject);
begin
  Calculate;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    ReadFromIni;
    FActivated := true;
  end;
end;

procedure TMainForm.CalcCompleteHandler(Sender: TObject;
  AMsg: String; isOK: Boolean);
begin
  if isOK then
    LblAccuracy.Font.Color := clWindowText else
    LblAccuracy.Font.Color := clRed;
  LblAccuracy.Caption := AMsg;
end;

procedure TMainForm.Calculate;
var
  msg: String;
  C: TWinControl;
  frame: TBaseFrame;
begin
  frame := FindFrame(PageControl.ActivePage);
  if frame = nil then
    exit;

  frame.OnCalcComplete := @CalcCompleteHandler;
  if frame.ValidData(msg, C) then begin
    frame.Calculate;
    if frame.ErrMsg <> '' then
      MessageDlg(frame.ErrMsg, mtError,[mbOK], 0);
  end
  else begin
    LblAccuracy.Caption := '';
    if C <> nil then C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
  end;
end;

function TMainForm.FindFrame(APage: TTabSheet): TBaseFrame;
var
  i: Integer;
  c: TControl;
begin
  for i:=0 to APage.ControlCount-1 do begin
    c := APage.Controls[i];
    if c is TBaseFrame then begin
      Result := TBaseFrame(c);
      exit;
    end;
  end;
  Result := nil;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteToIni;
    except
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  x: Integer;
begin
  x := lblAccuracy.Canvas.TextWidth('Line center-to-plane dist. (d)');

  // before TPlanarCapFrame to avoid naming conflict
  with TLinePlaneCapFrame.Create(self) do begin
    EditLeft := x; // + 4*ControlDist;
    Parent := PgLinePlane;
    Align := alClient;
  end;

  with TPlanarCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgPlanar;
    Align := alClient;
  end;

  with TCylindricalCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgCylindrical;
    Align := alClient;
  end;

  // must be created after CylindricalCapFrame, otherwise naming conflict...
  with TSphericalCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgSpherical;
    Align := alClient;
  end;

  // before TCylPlaneCapFrame !
  with TTwoCylCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := Pg2Cyl;
    Align := alClient;
  end;

  with TCylPlaneCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgCylPlane;
    Align := alClient;
  end;

  with TSeriesCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgSeries;
    Align := alClient;
  end;

  with TParallelCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgParallel;
    Align := alClient;
  end;

  with TPNJunctionCapFrame.Create(self) do begin
    EditLeft := x + 4*ControlDist;
    if Width > self.ClientWidth then
      self.ClientWidth := Width;
    Parent := PgPNJunction;
    Align := alClient;
  end;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  LblAccuracy.Caption := '';
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  frame: TBaseFrame;
  i: Integer;
  L, T, W, H: Integer;
  R: TRect;
  s: String;
begin
  ini := CreateIni;
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('Mainform', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);

    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(ini.ReadInteger('MainForm', 'WindowState', 0));

    PageControl.TabIndex := 0;
    s := ini.ReadString('MainForm', 'Page', '');
    if s <> '' then
      for i:=0 to PageControl.PageCount-1 do
        if s = PageControl.Pages[i].Caption then begin
          PageControl.ActivePage := PageControl.Pages[i];
          break;
        end;

    for i:=0 to PageControl.PageCount-1 do
    begin
      frame := FindFrame(PageControl.Pages[i]);
      if frame <> nil then
        frame.ReadfromIni(ini);
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteToIni;
var
  i: Integer;
  ini: TCustomIniFile;
  frame: TBaseFrame;
begin
  ini := CreateIni;
  try
    ini.WriteInteger('MainForm', 'Top', RestoredTop);
    ini.WriteInteger('MainForm', 'Left', RestoredLeft);
    ini.WriteInteger('MainForm', 'Width', RestoredWidth);
    ini.WriteInteger('MainForm', 'Height', RestoredHeight);
    ini.WriteInteger('MainForm', 'WindowState', Integer(WindowState));

    ini.WriteString('MainForm', 'Page', PageControl.Activepage.Caption);
    for i:=0 to PageControl.PageCount-1 do begin
      frame := FindFrame(PageControl.Pages[i]);
      if frame <> nil then
        frame.WriteToIni(ini);
    end;
    if ini is TMemIniFile then ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.BeforeRun;
begin

end;

end.

