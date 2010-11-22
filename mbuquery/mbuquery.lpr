program mbuquery;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },
  blogger, mbubase, mbuserial, mbutcp, mbustdq, cmdlparse;

const
  Prog_Version_Major 	= 1;
  Prog_Version_Minor 	= 3;
  prog_Abbr		= 'mbuquery';

type

  TQuerySetup = procedure(AQuery: TMBQuery) of object;

  { TQueryCmd }

  TQueryCmd = class
  private
    FQClass	: TMBQueryClass;
    FQSetup	: TQuerySetup;
  public
    constructor Create(AQClass: TMBQueryClass; AQSetup: TQuerySetup);
  end;


  { TQueryApp }

  TQueryApp = class(TCustomApplication)
  private
    mbc		: TMBClient;
    mbs		: TMBRTUMaster;
    mbt		: TMBTCPClient;
    mbq 	: TMBQuery;
    cmdl	: TCommandLineParser;
    FCommands	: TStringList;
    FArgCnt	: integer;
    FDoRun	: boolean;
    function CheckModbusError(AQuery: TMBQuery): boolean;
    function GetCmdDesc(AIndex: integer): TQueryCmd;
    function LookupCommand(const ACommand: string): integer;
    procedure SetupBitRead(AQuery: TMBQuery);
    procedure SetupRegRead(AQuery: TMBQuery);
    procedure SetupRegWrite(AQuery: TMBQuery);
  protected
    procedure Run;
    procedure PrintUsage;
    procedure OnBitQueryDone(AQuery: TMBQuery);
    procedure OnRegQueryDone(AQuery: TMBQuery);
    procedure OnRegWriteDone(AQuery: TMBQuery);
  public
    FOptProto	: TCommandLineString;
    FOptSlaveAddr : TCommandLineInteger;
    FOptRepCnt  : TCommandLineInteger;
    FOptDataType: TCommandLineString;
    FOptRadix	: TCommandLineString;
    FOptWaitMS  : TCommandLineInteger;
    FOptHelp 	: TCommandLineFlag;
    FOptVersion	: TCommandLineFlag;

    FOptDevice  : TCommandLineString;
    FOptBaudrate: TCommandLineInteger;
    FOptDatabits: TCommandLineInteger;
    FOptStopbits: TCommandLineInteger;
    FOptParity  : TCommandLineString;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property CmdDesc[AIndex: integer]: TQueryCmd read GetCmdDesc;
  end;

var
  Application: TQueryApp;

{ TQueryCmd }

constructor TQueryCmd.Create(AQClass: TMBQueryClass; AQSetup: TQuerySetup);
begin
  inherited Create;
  FQClass := AQClass;
  FQSetup := AQSetup;
end;


{ TQueryApp }

procedure TQueryApp.SetupBitRead(AQuery: TMBQuery);
var
  q : TMBQCustomReadBits;
begin
  if cmdl.Arguments.Count - FArgCnt < 2 then
    raise Exception.CreateFmt('At least two arguments required for command %s.',
      [cmdl.Arguments[FArgCnt - 1]]);

  q := TMBQCustomReadBits(AQuery);
  q.StartBit := StrToInt(cmdl.Arguments[FArgCnt]);
  q.BitCnt := StrToInt(cmdl.Arguments[FArgCnt + 1]);
  inc(FArgCnt, 2);
  q.OnRxDone := @OnBitQueryDone;
end;

procedure TQueryApp.OnBitQueryDone(AQuery: TMBQuery);
begin
  if not CheckModbusError(AQuery) then
    exit;
  writeln('Display of bits not yet implemented');
end;

procedure TQueryApp.SetupRegRead(AQuery: TMBQuery);
var
  q : TMBQCustomRWRegisters;
begin
  if cmdl.Arguments.Count - FArgCnt < 2 then
    raise Exception.CreateFmt('At least two arguments required for command %s.',
      [cmdl.Arguments[FArgCnt - 1]]);

  q := TMBQCustomRWRegisters(AQuery);
  q.rq.StartReg := StrToInt(cmdl.Arguments[FArgCnt]);
  q.rq.RegCnt := StrToInt(cmdl.Arguments[FArgCnt + 1]);
  inc(FArgCnt, 2);
  q.OnRxDone := @OnRegQueryDone;
end;

procedure TQueryApp.OnRegQueryDone(AQuery: TMBQuery);
var
  i	: integer;
  q	: TMBQCustomRWRegisters;
  dt, fmt	: string;
begin
  if not CheckModbusError(AQuery) then
    exit;

  dt := FOptDataType.Value;
  if FOptRadix.Value = '16' then
    fmt := '%x'
  else
    if dt[1] = 'u' then
      fmt := '%u'
    else
      fmt := '%d';
  q := AQuery as TMBQCustomRWRegisters;
  if dt = 'u8' then begin
    for i := 0 to pred(q.rq.RegCnt * 2) do
      writeln(Format('[%u.%u]: ' + fmt, [q.rq.StartReg + i div 2, i and 1, q.rp.RegU8[i]]));
  end else if dt = 'u16be' then begin
    for i := 0 to pred(q.rq.RegCnt) do
      writeln(Format('[%u]: ' + fmt, [q.rq.StartReg + i, q.rp.RegU16BE[i]]));
  end else if dt = 'u32be' then begin
    for i := 0 to pred(q.rq.RegCnt div 2) do
      writeln(Format('[%u]: ' + fmt, [q.rq.StartReg + i * 2, q.rp.RegU32BE[i]]));
  end else if dt = 's8' then begin
    for i := 0 to pred(q.rq.RegCnt * 2) do
      writeln(Format('[%u.%u]: ' + fmt, [q.rq.StartReg + i div 2, i and 1, q.rp.RegS8[i]]));
  end else if dt = 's16be' then begin
    for i := 0 to pred(q.rq.RegCnt) do
      writeln(Format('[%u]: ' + fmt, [q.rq.StartReg + i, q.rp.RegS16BE[i]]));
  end else if dt = 's32be' then begin
    for i := 0 to pred(q.rq.RegCnt div 2) do
      writeln(Format('[%u]: ' + fmt, [q.rq.StartReg + i * 2, q.rp.RegS32BE[i]]));
  end;
end;

procedure TQueryApp.SetupRegWrite(AQuery: TMBQuery);
var
  q : TMBQWriteHoldingRegisters;
  i, nvals : integer;
  dt	: string;
begin
  nvals := cmdl.Arguments.Count - FArgCnt;
  if nvals < 3 then
    raise Exception.CreateFmt('At least three arguments required for command %s.',
      [cmdl.Arguments[FArgCnt - 1]]);

  dec(nvals, 2);

  dt := FOptDataType.Value;
  q := TMBQWriteHoldingRegisters(AQuery);
  q.OnRxDone := @OnRegWriteDone;
  q.rq.StartReg := StrToInt(cmdl.Arguments[FArgCnt]);
  q.rq.RegCnt := StrToInt(cmdl.Arguments[FArgCnt + 1]);
  if dt = 'u16be' then begin
    for i := 0 to pred(q.rq.RegCnt) do begin
      q.rq.RegU16BE[i] := StrToInt(cmdl.Arguments[FArgCnt + i + 2]);
    end;
    inc(FArgCnt, q.rq.RegCnt + 2);
  end else
    raise Exception.CreateFmt('Data format ''%s'' not supported', [dt]);
end;

procedure TQueryApp.OnRegWriteDone(AQuery: TMBQuery);
var
  q : TMBQCustomRWRegisters;
begin
  if not CheckModbusError(AQuery) then
    exit;
  q := AQuery as TMBQCustomRWRegisters;
  writeln(Format('Write completed: start=%u, n=%u', [q.rp.StartReg, q.rp.RegCnt]));
end;

procedure TQueryApp.PrintUsage;
begin
  writeln('Usage: ' + prog_Abbr + ' {options} command [arguments]');
  writeln('Where options are:');
  writeln(cmdl.OptionsHelp);
  writeln('List of supported commands:');
  writeln('rdcoils first ncoils     Read Coil Status');
  writeln('rddisc first ninputs     Read Input Status');
  writeln('rdhold first nregs       Read Holding Registers');
  writeln('rdinput first nregs      Read Input Registers');
  writeln('wrhold first nregs data  Write Holding Registers');
end;

function TQueryApp.CheckModbusError(AQuery: TMBQuery):boolean;
begin
  result := false;
  if AQuery.Error <> mbeNoError then begin
    blogger.log.add(bllError, 'Modbus error: %s.', [mbu_ErrorToStr(AQuery.Error)]);
    exit;
  end;
  result := true;
end;

function TQueryApp.GetCmdDesc(AIndex: integer): TQueryCmd;
begin
  result := TQueryCmd(FCommands.Objects[AIndex]);
end;

function TQueryApp.LookupCommand(const ACommand: string):integer;
begin
  result := FCommands.IndexOf(ACommand);
  if result < 0 then
    exit;
  mbq := CmdDesc[result].FQClass.Create(FOptSlaveAddr.Value);
end;

procedure TQueryApp.Run;
var
  fnid, nr, waitms : integer;
  cmd	: string;
begin

  if FOptRepCnt.Value = 1 then
    waitms := 0
  else
    waitms := FOptWaitMS.Value;

  if not mbc.Connect then
    raise Exception.Create('Unable to connect');
  for nr := 0 to pred(FOptRepCnt.Value) do begin
    FArgCnt := 0;
    while FArgCnt < cmdl.Arguments.Count do begin
      cmd := cmdl.Arguments[FArgCnt];
      fnid := LookupCommand(cmd);
      if fnid = -1 then begin
        writeln(stderr, 'Unknown command: ', cmd);
        break;
      end;
      inc(FArgCnt);
      CmdDesc[fnid].FQSetup(mbq);
      mbc.SubmitQuery(mbq);
      mbq.WaitForCompletion;
      mbc.GetCompletedQuery;
      mbq.OnRxDone(mbq);
      mbq.Free;
      if waitms <> 0 then
        sleep(waitms);
    end;
  end;
end;

constructor TQueryApp.Create(TheOwner: TComponent);
var
  lc	: TBLogConsumer;
  host, port : string;
  i	: integer;
begin
  inherited Create(TheOwner);
  FDoRun := false;
  StopOnException := True;
  mbs := nil;
  mbt := nil;
  mbq := nil;

  // setup console logger
  blogger.log.ProcName := prog_Abbr;
  lc := TBLogToConsole.Create;
  lc.Level := bllError;
  blogger.Log.RegisterConsumer(lc);

  // list of supported queries
  FCommands := TStringList.Create;
  FCommands.AddObject(TMBQReadCoils.ShortName, TQueryCmd.Create(TMBQReadCoils, @SetupBitRead));
  FCommands.AddObject(TMBQReadDiscreteInputs.ShortName, TQueryCmd.Create(TMBQReadDiscreteInputs, @SetupBitRead));
  FCommands.AddObject(TMBQReadHoldingRegisters.ShortName, TQueryCmd.Create(TMBQReadHoldingRegisters, @SetupRegRead));
  FCommands.AddObject(TMBQReadInputRegisters.ShortName, TQueryCmd.Create(TMBQReadInputRegisters, @SetupRegRead));
  FCommands.AddObject(TMBQWriteHoldingRegisters.ShortName, TQueryCmd.Create(TMBQWriteHoldingRegisters, @SetupRegWrite));

  // prepare command line parser
  cmdl := TCommandLineParser.Create;
  cmdl.ProgramName := prog_Abbr;


  FOptHelp := TCommandLineFlag.Create		(cmdl, 'help', 'h', 'Print this help');

  FOptProto := TCommandLineString.Create	(cmdl, 'protocol', 'm', 'Protocol', 'rtu', ['rtu', 'tcp']);
  FOptSlaveAddr := TCommandLineInteger.Create	(cmdl, 'addr', 'a', 'Slave address', 1, 1, 255);
  FOptRepCnt := TCommandLineInteger.Create	(cmdl, 'repeat', 'n', 'Number of repeats', 1, 1, 100000);
  FOptWaitMS := TCommandLineInteger.Create	(cmdl, 'waitms', 'w',
    'Sleep for <int> milleseconds between queries', 1000, 1, 1000000);

  FOptDataType := TCommandLineString.Create	(cmdl, 'datatype', 't', 'Treat values as', 'u16be',
    ['s8', 's16be', 's32be', 'u8', 'u16be', 'u32be']);
  FOptRadix := TCommandLineString.Create	(cmdl, 'datatype', 'x', 'Display values in the given base', '10',
    ['10', '16']);

  FOptVersion := TCommandLineFlag.Create	(cmdl, 'version', 'v', 'Print program version and exit');

  FOptDevice := TCommandLineString.Create	(cmdl, 'device', 'd', 'Device or host[:port] name');
  FOptBaudrate := TCommandLineInteger.Create	(cmdl, 'baudrate', 'b', 'Baudrate', 19200);
  FOptDatabits := TCommandLineInteger.Create	(cmdl, 'nbits', #0, 'Number of data bits', 8, 7, 8);
  FOptStopbits := TCommandLineInteger.Create	(cmdl, 'stopbits', 's', 'Number of stop bits', 1, 1, 2);
  FOptParity := TCommandLineString.Create	(cmdl, 'parity', 'p', 'Parity', 'none', ['none', 'even', 'odd']);
  cmdl.Parse;

  if FOptHelp.Present then begin
    PrintUsage;
    exit;
  end;

  if FOptVersion.Present then begin
    writeln(Format('%s version %d.%d. Library version %s.', [cmdl.ProgramName,
      Prog_Version_Major, Prog_Version_Minor, mbu_Version]));
    exit;
  end;

  if not FOptDevice.Present then begin
    writeln('Device or host name must be provided.');
    exit;
  end;

  if cmdl.Arguments.Count < 1 then begin
    writeln('At least one command must be provided.');
    exit;
  end;

  if FOptProto.Value = 'rtu' then begin
    mbs := TMBRTUMaster.Create;
  end else if FOptProto.Value = 'tcp' then begin
    host := FOptDevice.Value;
    port := '';
    i := Pos(':', host);
    if i <> 0 then begin
      port := Copy(host, i + 1, 20);
      Delete(host, i, Length(host));
    end;
    mbt := TMBTCPClient.Create;
    mbt.TargetHost := host;
    if port <> '' then
      mbt.TargetPort := port;
    mbc := mbt;
  end;

  if mbs <> NIL then begin
    mbs.PortName := FOptDevice.Value;
    mbs.Baudrate := FOptBaudrate.Value;
    mbs.DataBits := FOptDatabits.Value;
    mbs.StopBits := FOptStopbits.Value;

    if FOptParity.Value = 'none' then
      mbs.Parity := 'N'
    else if FOptParity.Value = 'even' then
      mbs.Parity := 'E'
    else
      mbs.Parity := 'O';
    mbc := mbs;
  end;

  FDoRun := true;
end;

destructor TQueryApp.Destroy;
begin
  FreeAndNil(cmdl);
  FreeAndNil(mbc);
  FreeAndNil(FCommands);
  inherited Destroy;
end;

{$IFDEF WINDOWS}{$R mbuquery.rc}{$ENDIF}

begin
  Application := TQueryApp.Create(nil);
  Application.Title := 'Modbus Utils - Query';
  try
    if Application.FDoRun then
      Application.Run;
  finally
    Application.Free;
  end;
end.

