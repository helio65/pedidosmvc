unit Dao.Usuario;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Dao.Conexao, Model.Usuario, Framework.Factory;

type
  TUsuarioDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(AUsuarioModel : TUsuarioModel) : Boolean;
    function Update(AUsuarioModel : TUsuarioModel) : Boolean;
    function Delete(co_usuario : Integer) : Boolean;
    function ValidateUser(AUserName : String) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll: TObjectList<TUsuarioModel>;
    function GetDateTime : TDateTime;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TUsuarioDao }

constructor TUsuarioDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TUsuarioDao.Delete(co_usuario: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM USUARIO               ')
            .AppendLine('  WHERE CO_USUARIO = :CO_USUARIO; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_USUARIO').AsInteger := co_usuario;
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

destructor TUsuarioDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TUsuarioDao.GetAll: TObjectList<TUsuarioModel>;
var
  LUsuarios : TUsuarioModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_USUARIO, NM_USUARIO, TX_LOGIN, TX_SENHA, IN_ATIVO, ')
            .AppendLine('       DT_CADASTRO, DT_ALTERACAO                             ')
            .AppendLine('  FROM USUARIO                                               ')
            .AppendLine('  ORDER BY NM_USUARIO;                                       ');

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
  Result := TObjectList<TUsuarioModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LUsuarios             := TUsuarioModel.Create;
      LUsuarios.co_usuario  := Self.FQuery.FieldByName('CO_USUARIO').AsInteger;
      LUsuarios.nm_usuario  := Self.FQuery.FieldByName('NM_USUARIO').AsString;
      LUsuarios.tx_login    := Self.FQuery.FieldByName('TX_LOGIN').AsString;
      LUsuarios.tx_senha    := Self.FQuery.FieldByName('TX_SENHA').AsString;
      LUsuarios.in_ativo    := Self.FQuery.FieldByName('IN_ATIVO').AsString;
      LUsuarios.dt_cadastro := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      if not Self.FQuery.FieldByName('DT_ALTERACAO').IsNull then
        LUsuarios.dt_alteracao    := Self.FQuery.FieldByName('DT_ALTERACAO').AsDateTime;
      Result.Add(LUsuarios);
      Self.FQuery.Next;
    end;
  end;
end;

function TUsuarioDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TUsuarioDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TUsuarioDao.Insert(AUsuarioModel: TUsuarioModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('INSERT INTO USUARIO (CO_USUARIO, NM_USUARIO, TX_LOGIN, TX_SENHA, IN_ATIVO)       ')
            .AppendLine('             VALUES (:CO_USUARIO, :NM_USUARIO, :TX_LOGIN, :TX_SENHA, :IN_ATIVO); ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_USUARIO').AsInteger := AUsuarioModel.co_usuario;
  Self.FQuery.ParamByName('NM_USUARIO').AsString  := AUsuarioModel.nm_usuario;
  Self.FQuery.ParamByName('TX_LOGIN').AsString    := AUsuarioModel.tx_login;
  Self.FQuery.ParamByName('TX_SENHA').AsString    := Self.FFactory.GetStrHashSHA512_256(AUsuarioModel.tx_senha);
  Self.FQuery.ParamByName('IN_ATIVO').AsString    := AUsuarioModel.in_ativo;    

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

function TUsuarioDao.Update(AUsuarioModel: TUsuarioModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('UPDATE USUARIO                     ')
            .AppendLine('  SET NM_USUARIO   = :NM_USUARIO,  ')
            .AppendLine('      IN_ATIVO     = :IN_ATIVO,    ')
            .AppendLine('      DT_ALTERACAO = :DT_ALTERACAO ')
            .AppendLine('  WHERE CO_USUARIO = :CO_USUARIO;  ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                               := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NM_USUARIO').AsString     := AUsuarioModel.nm_usuario;
  Self.FQuery.ParamByName('IN_ATIVO').AsString       := AUsuarioModel.in_ativo;
  Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime := AUsuarioModel.dt_alteracao;
  Self.FQuery.ParamByName('CO_USUARIO').AsInteger    := AUsuarioModel.co_usuario;

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

function TUsuarioDao.ValidateUser(AUserName: String): Boolean;
begin
  Result := False;
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT TX_LOGIN               ')
            .AppendLine('  FROM USUARIO;               ')
            .AppendLine('  WHERE TX_LOGIN = :TX_LOGIN; ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  Self.FQuery.ParamByName('TX_LOGIN').AsString := AUserName;
  
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    if Self.FQuery.RowsAffected > 0 then
      Result := True;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
end;

end.
