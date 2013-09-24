program mbuproxy;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
{$IFDEF WIN32}
  ,Windows
{$ENDIF}
  { you can add units after this },
  blogger, mbubase, mbuserial, mbutcp, mbutcpsrv, mbustdq, cmdlparse;

const
  Prog_Version_Major =	1;
  Prog_Version_Minor = 	3;

  progAbbr: string = 'mbuproxy';

type

  { TMBUProxy }

  TMBUProxy = class(TCustomApplication)
  private
    cmdl	: TCommandLineParser;
    FRoutes 	: TList;
    FClients	: TMBClientList;
    FServers	: TMBServerList;

    function CheckModbusError(AQuery: TMBQuery): boolean;
    procedure ClientCommand(const AParams: pchar);
    procedure ClientSerialCommand(AParams: pchar);
    procedure ClientTCPCommand(AParams: pchar);
    function LookupRoute(AQuery: TMBQuery): TMBRoute;
    procedure RouteCommand(AParams: pchar);
    procedure ServerCommand(const AParams: pchar);
    procedure ServerTCPCommand(AParams: pchar);
    procedure FServersServersReceive(AQuery: TMBQuery);
  protected
    procedure DoRun; override;
    procedure PrintUsageAndExit;
    procedure errx(const AReason: string);
  public
    FOptCfgFile   : TCommandLineString;
    FOptSlaveAddr : TCommandLineInteger;
    FOptStartReg  : TCommandLineInteger;
    FOptRepCnt    : TCommandLineInteger;
    FOptFunction  : TCommandLineString;
    FOptDataType  : TCommandLineString;
    FOptRadix	  : TCommandLineString;
    FOptCount     : TCommandLineInteger;
    FOptWaitMS    : TCommandLineInteger;
    FOptHelp 	: TCommandLineFlag;
    FOptVersion	: TCommandLineFlag;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleException(ASender: TObject);override;
  end;

var
  Application: TMBUProxy;



{ TMBUPoll }

procedure TMBUProxy.PrintUsageAndExit;
begin
  writeln('Usage: mbuproxy {options}');
  writeln('Where options are:');
  writeln(cmdl.OptionsHelp);
  Terminate;
end;

function TMBUProxy.CheckModbusError(AQuery: TMBQuery):boolean;
begin
  result := false;
  if AQuery.Error <> mbeNoError then begin
    writeln('Modbus error: ', mbu_ErrorToStr(AQuery.Error));
    exit;
  end;
  result := true;
end;

procedure TMBUProxy.errx(const AReason: string);
begin
  Writeln('Error: ', AReason);
  Terminate;
end;

procedure TMBUProxy.HandleException(ASender: TObject);
begin
  inherited HandleException(ASender);
end;

procedure TMBUProxy.ClientSerialCommand(AParams: pchar);
var
  conf		: TCommandLineParser;
  OptName	: TCommandLineString;
  OptProto	: TCommandLineString;
  OptDevice	: TCommandLineString;
  OptBaudrate  : TCommandLineInteger;
  OptDatabits  : TCommandLineInteger;
  OptStopbits  : TCommandLineInteger;
  OptParity    : TCommandLineString;
  OptTimeout	: TCommandLineInteger;
  mbs		: TMBRTUMaster;
  cp		: pchar;
begin
  cp := AParams;
  conf := TCommandLineParser.Create(false);
  conf.ProgramName := 'client-serial';
  OptName := TCommandLineString.Create		(conf, 'name', #0, 'Client name');
  OptName.Required := true;
  OptProto := TCommandLineString.Create		(conf, 'proto', #0, 'Protocol', 'rtu', ['rtu']);
  OptDevice := TCommandLineString.Create	(conf, 'device', #0, 'Serial device name');
  OptDevice.Required := true;
  OptBaudrate := TCommandLineInteger.Create	(conf, 'baudrate', #0, 'Baudrate', 19200);
  OptDatabits := TCommandLineInteger.Create	(conf, 'nbits', #0, 'Number of data bits', 8, 7, 8);
  OptStopbits := TCommandLineInteger.Create	(conf, 'stopbits', #0, 'Number of stop bits', 1, 1, 2);
  OptParity := TCommandLineString.Create	(conf, 'parity', #0, 'Parity', 'none', ['none', 'even', 'odd']);
  OptTimeout := TCommandLineInteger.Create	(conf, 'timeout', #0, 'Reply timeout (ms)', 1000, 100);
  conf.Parse(cp);

  if OptProto.Value = 'rtu' then begin
    mbs := TMBRTUMaster.Create;
  end;

  mbs.PortName := OptDevice.Value;
  mbs.Baudrate := OptBaudrate.Value;
  mbs.DataBits := OptDatabits.Value;
  mbs.StopBits := OptStopbits.Value;

  if OptParity.Value = 'none' then
    mbs.Parity := 'N'
  else if OptParity.Value = 'even' then
    mbs.Parity := 'E'
  else
    mbs.Parity := 'O';
  FClients.Add(OptName.Value, mbs);
  conf.Free;
end;

procedure TMBUProxy.ClientTCPCommand(AParams: pchar);
var
  conf		: TCommandLineParser;
  OptName	: TCommandLineString;
  OptProto	: TCommandLineString;
  OptTarget	: TCommandLineString;
  OptPort	: TCommandLineString;
  OptTimeout	: TCommandLineInteger;
  mbt		: TMBTCPClient;
  cp		: pchar;
begin
  cp := AParams;
  conf := TCommandLineParser.Create(false);
  conf.ProgramName := 'client-tcp';
  OptName := TCommandLineString.Create		(conf, 'name', #0, 'Client name');
  OptName.Required := true;
  OptProto := TCommandLineString.Create		(conf, 'proto', #0, 'Protocol', 'tcp', ['tcp']);
  OptTarget := TCommandLineString.Create	(conf, 'target', #0, 'Target host');
  OptTarget.Required := true;
  OptPort := TCommandLineString.Create		(conf, 'port', #0, 'Target port');
  OptTimeout := TCommandLineInteger.Create	(conf, 'timeout', #0, 'Reply timeout (ms)', 2000, 100);
  conf.Parse(cp);

//  if OptProto.Value = 'rtu' then begin
    mbt := TMBTCPClient.Create;
    mbt.TargetHost := OptTarget.Value;
    mbt.TargetPort := OptPort.Value;
//  end;
  FClients.Add(OptName.Value, mbt);
  conf.Free;
end;

procedure TMBUProxy.ClientCommand(const AParams: pchar);
var
  ctype	: string;
  cp	: pchar;
begin
  cp  := AParams;

  ctype := TCommandLineParser.GetToken(cp, cp);

  if ctype = '' then
    raise Exception.Create('No client type provided.');

  if ctype = 'serial' then begin
    ClientSerialCommand(cp)
  end else if ctype = 'tcp' then begin
    ClientTCPCommand(cp)
  end else
    raise Exception.CreateFmt('Client of type ''%s'' not supported.', [ctype]);
end;

procedure TMBUProxy.ServerTCPCommand(AParams: pchar);
var
  conf		: TCommandLineParser;
  OptName	: TCommandLineString;
  OptProto	: TCommandLineString;
  OptListenOn	: TCommandLineString;
  OptPort	: TCommandLineString;
  OptTimeout	: TCommandLineInteger;
  mbs		: TMBTCPServer;
  cp		: pchar;
begin
  cp := AParams;
  conf := TCommandLineParser.Create(false);
  conf.ProgramName := 'server-tcp';
  OptName := TCommandLineString.Create		(conf, 'name', #0, 'Client name');
  OptName.Required := true;
  OptProto := TCommandLineString.Create		(conf, 'proto', #0, 'Protocol', 'tcp', ['tcp']);
  OptListenOn := TCommandLineString.Create		(conf, 'listenon', #0, 'Address to listen on');
  OptListenOn.Required := true;
  OptPort := TCommandLineString.Create		(conf, 'port', #0, 'Port to listen on');
  OptTimeout := TCommandLineInteger.Create	(conf, 'timeout', #0, 'Reply timeout (ms)', 2000, 100);
  conf.Parse(cp);

  if OptProto.Value = 'tcp' then begin
    mbs := TMBTCPServer.Create;
    mbs.LocalHost := OptListenOn.Value;
    mbs.LocalPort := OptPort.Value;
  end;
//  mbs.DumpPackets := true;

  FServers.Add(OptName.Value, mbs);
  conf.Free;
end;

procedure TMBUProxy.ServerCommand(const AParams: pchar);
var
  stype	: string;
  cp	: pchar;
begin
  cp  := AParams;

  stype := TCommandLineParser.GetToken(cp, cp);

  if stype = '' then
    raise Exception.Create('No server type provided.');

  if stype = 'tcp' then begin
    ServerTCPCommand(cp)
  end else
    raise Exception.CreateFmt('Server of type ''%s'' not supported.', [stype]);
end;

procedure TMBUProxy.RouteCommand(AParams: pchar);
var
  conf		: TCommandLineParser;
  OptFrom	: TCommandLineString;
  OptTo		: TCommandLineString;
  OptFirst	: TCommandLineInteger;
  OptLast	: TCommandLineInteger;
  mbs		: TMBServer;
  mbc		: TMBClient;
  mbr		: TMBRoute;
  cp		: pchar;
begin
  cp := AParams;
  conf := TCommandLineParser.Create(false);
  conf.ProgramName := 'route';
  OptFrom := TCommandLineString.Create		(conf, 'from', #0, 'Server name');
  OptFrom.Required := true;
  OptTo := TCommandLineString.Create		(conf, 'to', #0, 'Client name');
  OptTo.Required := true;
  OptFirst := TCommandLineInteger.Create	(conf, 'first', #0, 'First slave in range', 0, 0, 255);
  OptLast := TCommandLineInteger.Create		(conf, 'last', #0, 'Last slave in range', 255, 0, 255);
  conf.Parse(cp);

  mbs := FServers.LookupByName(OptFrom.Value);
  if mbs = nil then
    raise Exception.CreateFmt('Invalid server name ''%s''.', [OptFrom.Value]);

  mbc := FClients.LookupByName(OptTo.Value);
  if mbc = nil then
    raise Exception.CreateFmt('Invalid client name ''%s''.', [OptTo.Value]);

  if OptFirst.Value > OptLast.Value then
    raise Exception.Create('Value of ''first'' are greater than ''last''');

  mbr := TMBRoute.Create(mbs, mbc, OptFirst.Value, OptLast.Value);
  FRoutes.Add(mbr);
  conf.Free;
end;

function TMBUProxy.LookupRoute(AQuery: TMBQuery): TMBRoute;
var
  mbr	: TMBRoute;
  i	: integer;
begin
  for i := 0 to pred(FRoutes.Count) do begin
    mbr := TMBRoute(FRoutes[i]);
    if (mbr.Server = AQuery.Client.Owner) and
       (AQuery.SlaveAddr >= mbr.First) and (AQuery.SlaveAddr <= mbr.Last) then begin
      result := mbr;
      exit;
    end;
  end;
  result := nil;
end;

// Note: this procedure called asynchonously
procedure TMBUProxy.FServersServersReceive(AQuery: TMBQuery);
var
  mbr	: TMBRoute;
  q	: TMBQuery;
  i	: integer;
begin
  mbr := LookupRoute(AQuery);
  if mbr = nil then begin
    AQuery.Free;	// discard requests for unknown destinations
    exit;
  end;
  AQuery.Route := mbr;
  mbr.Client.SubmitQuery(AQuery);
  i := 0;
  while i < 10 do begin
    mbr.Client.SignalEvent.WaitFor(1000);
    q := mbr.Client.GetCompletedQuery;
    if q <> nil then
      break;
  end;
  if q <> nil then	// TODO: report timeout error
    q.Route.Server.SubmitReply(q);
end;

procedure TMBUProxy.DoRun;
var
  i	: integer;
  s, ft, cmd	: string;
  cfgfile	: text;
  cp	: pchar;
begin
  FOptHelp := TCommandLineFlag.Create		(cmdl, 'help', 'h', 'Print this help');

  FOptCfgFile := TCommandLineString.Create	(cmdl, 'conf', 'f', 'Configuration file');
  FOptCfgFile.Required := true;
  FOptVersion := TCommandLineFlag.Create	(cmdl, 'version', 'v', 'Print program version and exit');
  cmdl.Parse;

  if FOptHelp.Present then begin
    PrintUsageAndExit;
    exit;
  end;

  if FOptVersion.Present then begin
    writeln(Format('%s version %d.%d', [cmdl.ProgramName, Prog_Version_Major, Prog_Version_Minor]));
    Terminate;
    exit;
  end;

  System.Assign(cfgfile, FOptCfgFile.Value);
  System.Reset(cfgfile);
  ft := '';
  while not eof(cfgfile) or (ft <> '') do begin
    System.readln(cfgfile, s);
    if (length(s) = 0) or (s[1] = '#') then begin
      if ft = '' then
        continue;
    end else begin
      if ft = '' then begin
        ft := s;
        continue;
      end;
      if (s[1] in [#9, #32]) then begin
        AppendStr(ft, s);
        continue;
      end;
    end;
    cp := pchar(ft);
    cmd := TCommandLineParser.GetToken(cp, cp);
    if cmd = 'client' then
      ClientCommand(cp)
    else if cmd = 'server' then
      ServerCommand(cp)
    else if cmd = 'route' then
      RouteCommand(cp)
    else
      raise Exception.CreateFmt('Unknown command ''%s''', [cmd]);
    ft := s;
  end;
  System.Close(cfgfile);


  for i := 0 to pred(FServers.Count) do begin
    FServers[i].OnReceive  := @FServersServersReceive;
    FServers[i].Connect;
  end;

  for i := 0 to pred(FClients.Count) do
    FClients[i].Connect;

  try
    while true do begin
      Sleep(1000);
    end;
  finally
    Terminate;
  end;
end;

constructor TMBUProxy.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FRoutes := TList.Create;
  FClients := TMBClientList.Create;
  FServers := TMBServerList.Create;
  StopOnException:=True;
  cmdl := TCommandLineParser.Create;
  cmdl.ProgramName := progAbbr;
end;

destructor TMBUProxy.Destroy;
begin
  FreeAndNil(FClients);
  FreeAndNil(FServers);
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

{$IFDEF WINDOWS}{$R mbuproxy.rc}{$ENDIF}

var
  lc	: TBLogConsumer;

begin
  blogger.log.ProcName := progAbbr;
  lc := TBLogToConsole.Create;
  lc.Level := bllDebug;
  blogger.Log.RegisterConsumer(lc);
  Application := TMBUProxy.Create(nil);
  Application.Title := 'Modbus Utils - Proxy';
  Application.Run;
  Application.Free;
end.
