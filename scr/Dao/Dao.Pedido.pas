unit Dao.Pedido;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Dao.Conexao, Model.Pedido, Framework.Factory;

type
  TPedidoDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(APedidoModel : TPedidoModel) : Boolean;
    function Update(APedidoModel : TPedidoModel) : Boolean;
    function Delete(nu_pedido : Integer) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll: TObjectList<TPedidoModel>;
    function GetDateTime : TDateTime;
    procedure ToLoadComboCliente(AObject : TFDQuery);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPedidoDao }

constructor TPedidoDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TPedidoDao.Delete(nu_pedido: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM PEDIDO               ')
            .AppendLine('  WHERE NU_PEDIDO = :NU_PEDIDO; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NU_PEDIDO').AsInteger := nu_pedido;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

destructor TPedidoDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TPedidoDao.GetAll: TObjectList<TPedidoModel>;
var
  LPedidos : TPedidoModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT NU_PEDIDO, CO_EMPRESA, IN_SITUACAO,  ')
            .AppendLine('       VL_PEDIDO, DT_CADASTRO, DT_ALTERACAO ')
            .AppendLine('  FROM PEDIDO                               ')
            .AppendLine('  ORDER BY DT_CADASTRO;                     ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
  Result := TObjectList<TPedidoModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LPedidos             := TPedidoModel.Create;
      LPedidos.nu_pedido   := Self.FQuery.FieldByName('NU_PEDIDO').AsInteger;
      LPedidos.co_empresa  := Self.FQuery.FieldByName('CO_EMPRESA').AsInteger;
      LPedidos.in_situacao := Self.FQuery.FieldByName('IN_SITUACAO').AsString;
      LPedidos.vl_pedido   := Self.FQuery.FieldByName('VL_PEDIDO').AsFloat;
      LPedidos.dt_cadastro := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      if not Self.FQuery.FieldByName('DT_ALTERACAO').IsNull then
        LPedidos.dt_alteracao    := Self.FQuery.FieldByName('DT_ALTERACAO').AsDateTime;
      Result.Add(LPedidos);
      Self.FQuery.Next;
    end;
  end;
end;

function TPedidoDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TPedidoDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TPedidoDao.Insert(APedidoModel: TPedidoModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('INSERT INTO PEDIDO (NU_PEDIDO, CO_EMPRESA, IN_SITUACAO)      ')
            .AppendLine('             VALUES (:NU_PEDIDO, :CO_EMPRESA, :IN_SITUACAO); ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NU_PEDIDO').AsInteger  := APedidoModel.nu_pedido;
  Self.FQuery.ParamByName('CO_EMPRESA').AsInteger := APedidoModel.co_empresa;
  Self.FQuery.ParamByName('IN_SITUACAO').AsString := APedidoModel.in_situacao;

  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TPedidoDao.ToLoadComboCliente(AObject: TFDQuery);
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_EMPRESA, NM_RAZAO_SOCIAL ')
            .AppendLine('  FROM EMPRESA                     ')
            .AppendLine('  ORDER BY NM_RAZAO_SOCIAL         ');

  AObject.Close;
  AObject.SQL.Clear;
  AObject.Connection := DMConexao.Conexao;
  AObject.SQL.Text   := Self.FStrB.ToString;

  if not AObject.Connection.InTransaction then
    AObject.Connection.StartTransaction;
  try
    AObject.Open;
    AObject.Connection.Commit;
  except
    AObject.Connection.Rollback;
  end;
end;

function TPedidoDao.Update(APedidoModel: TPedidoModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('UPDATE PEDIDO                      ')
            .AppendLine('  SET IN_SITUACAO  = :IN_SITUACAO, ')
            .AppendLine('      DT_ALTERACAO = :DT_ALTERACAO ')
            .AppendLine('  WHERE NU_PEDIDO = :NU_PEDIDO;    ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                               := Self.FStrB.ToString;
  Self.FQuery.ParamByName('IN_SITUACAO').AsString    := APedidoModel.in_situacao;
  Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime := APedidoModel.dt_alteracao;
  Self.FQuery.ParamByName('NU_PEDIDO').AsInteger     := APedidoModel.nu_pedido;

  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

end.
