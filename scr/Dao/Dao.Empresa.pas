unit Dao.Empresa;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Dao.Conexao, Model.Empresa, Framework.Factory;

type
  TEmpresaDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(AEmpresaModel : TEmpresaModel) : Boolean;
    function Update(AEmpresaModel : TEmpresaModel) : Boolean;
    function Delete(co_empresa : Integer) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll: TObjectList<TEmpresaModel>;
    function GetDateTime : TDateTime;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEmpresaDao }

constructor TEmpresaDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TEmpresaDao.Delete(co_empresa: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM EMPRESA               ')
            .AppendLine('  WHERE CO_EMPRESA = :CO_EMPRESA; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_EMPRESA').AsInteger := co_empresa;
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

destructor TEmpresaDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TEmpresaDao.GetAll: TObjectList<TEmpresaModel>;
var
  LEmpresa : TEmpresaModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_EMPRESA, NM_RAZAO_SOCIAL, NU_CNPJ, TX_EMAIL, ')
            .AppendLine('       DT_CADASTRO, DT_ALTERACAO                       ')
            .AppendLine('  FROM EMPRESA                                         ')
            .AppendLine('  ORDER BY NM_RAZAO_SOCIAL;                            ');

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
  Result := TObjectList<TEmpresaModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LEmpresa                 := TEmpresaModel.Create;
      LEmpresa.co_empresa      := Self.FQuery.FieldByName('CO_EMPRESA').AsInteger;
      LEmpresa.nm_razao_social := Self.FQuery.FieldByName('NM_RAZAO_SOCIAL').AsString;
      LEmpresa.nu_cnpj         := Self.FQuery.FieldByName('NU_CNPJ').AsString;
      LEmpresa.tx_email        := Self.FQuery.FieldByName('TX_EMAIL').AsString;
      LEmpresa.dt_cadastro     := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      if not Self.FQuery.FieldByName('DT_ALTERACAO').IsNull then
        LEmpresa.dt_alteracao    := Self.FQuery.FieldByName('DT_ALTERACAO').AsDateTime;
      Result.Add(LEmpresa);
      Self.FQuery.Next;
    end;
  end;
end;

function TEmpresaDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TEmpresaDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TEmpresaDao.Insert(AEmpresaModel: TEmpresaModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('INSERT INTO EMPRESA (CO_EMPRESA, NM_RAZAO_SOCIAL, NU_CNPJ, TX_EMAIL)      ')
            .AppendLine('             VALUES (:CO_EMPRESA, :NM_RAZAO_SOCIAL, :NU_CNPJ, :TX_EMAIL); ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                                := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_EMPRESA').AsInteger     := AEmpresaModel.co_empresa;
  Self.FQuery.ParamByName('NM_RAZAO_SOCIAL').AsString := AEmpresaModel.nm_razao_social;
  Self.FQuery.ParamByName('NU_CNPJ').AsString         := AEmpresaModel.nu_cnpj;
  Self.FQuery.ParamByName('TX_EMAIL').AsString        := AEmpresaModel.tx_email;

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

function TEmpresaDao.Update(AEmpresaModel: TEmpresaModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('UPDATE EMPRESA                            ')
            .AppendLine('  SET NM_RAZAO_SOCIAL = :NM_RAZAO_SOCIAL, ')
            .AppendLine('      NU_CNPJ         = :NU_CNPJ,         ')
            .AppendLine('      TX_EMAIL        = :TX_EMAIL,        ')
            .AppendLine('      DT_ALTERACAO    = :DT_ALTERACAO     ')
            .AppendLine('  WHERE CO_EMPRESA = :CO_EMPRESA;         ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                                := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NM_RAZAO_SOCIAL').AsString := AEmpresaModel.nm_razao_social;
  Self.FQuery.ParamByName('NU_CNPJ').AsString         := AEmpresaModel.nu_cnpj;
  Self.FQuery.ParamByName('TX_EMAIL').AsString        := AEmpresaModel.tx_email;
  Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime  := AEmpresaModel.dt_alteracao;
  Self.FQuery.ParamByName('CO_EMPRESA').AsInteger     := AEmpresaModel.co_empresa;

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
