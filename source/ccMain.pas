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
  frame: TBaseFrame;
begin
  frame := FindFrame(PageControl.ActivePage);
  if Assigned(frame) then
  begin
    frame.OnCalcComplete := @CalcCompleteHandler;
    frame.CheckAndCalculate;
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
begin
  // The frames will be created when the corresponding tab becomes active.
  // We store the frame class to be created in the Tag of the TabSheet.
  PgPlanar.Tag := {%H-}PtrUInt(TPlanarCapFrame);
  PgLinePlane.Tag := {%H-}PtrUInt(TLinePlaneCapFrame);
  PgCylindrical.Tag := {%H-}PtrUInt(TCylindricalCapFrame);
  PgSpherical.Tag := {%H-}PtrUInt(TSphericalCapFrame);
  Pg2Cyl.Tag := {%H-}PtrUInt(TTwoCylCapFrame);
  PgCylPlane.Tag := {%H-}PtrUInt(TCylPlaneCapFrame);
  PgSeries.Tag := {%H-}PtrUInt(TSeriesCapFrame);
  PgParallel.Tag := {%H-}PtrUInt(TParallelCapFrame);
  PgPNJunction.Tag := {%H-}PtrUInt(TPNJunctionCapFrame);
end;

procedure TMainForm.PageControlChange(Sender: TObject);
var
  frame: TBaseFrame;
  x: Integer;
begin
  x := lblAccuracy.Canvas.TextWidth('Line center-to-plane dist. (d)');
  LblAccuracy.Caption := '';

  frame := FindFrame(PageControl.ActivePage);
  if frame = nil then
    with {%H-}TBaseFrameClass(PageControl.ActivePage.Tag).Create(Self) do
    begin
      Name := '';
      EditLeft := x + 4*ControlDist;
      if Width > self.ClientWidth then
        self.ClientWidth := Width;
      Parent := PageControl.ActivePage;
      Align := alClient;
    end;
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
    begin
      for i:=0 to PageControl.PageCount-1 do
        if s = PageControl.Pages[i].Caption then begin
          PageControl.ActivePage := PageControl.Pages[i];
          break;
        end;
      PageControlChange(nil);
    end;

    for i:=0 to PageControl.PageCount-1 do
    begin
      frame := FindFrame(PageControl.Pages[i]);
      if frame <> nil then
      begin
        frame.ReadFromIni(ini);
        if i = Pagecontrol.ActivePageIndex then
          frame.CheckAndCalculate;
      end;
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

