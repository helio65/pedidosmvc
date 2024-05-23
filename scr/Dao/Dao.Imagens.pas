unit Dao.Imagens;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Vcl.Imaging.jpeg, Data.DB, Dao.Conexao,
  Model.Imagens, Framework.Factory;

type
  TImagensDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(AImagensModel : TImagensModel) : Boolean;
    function Delete(co_imagem : Integer) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll(co_produto : Integer): TObjectList<TImagensModel>;
    function GetDateTime : TDateTime;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TImagensDao }

constructor TImagensDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TImagensDao.Delete(co_imagem: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM PRODUTO_IMAGEM      ')
            .AppendLine('  WHERE CO_IMAGEM = :CO_IMAGEM; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_IMAGEM').AsInteger := co_imagem;
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

destructor TImagensDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TImagensDao.GetAll(co_produto : Integer): TObjectList<TImagensModel>;
var
  LImagens : TImagensModel;
  LJpg : TJpegImage;
  LStream : TMemoryStream;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_IMAGEM, CO_PRODUTO, IM_PRODUTO, TX_EXTENSAO, ')
            .AppendLine('       DT_CADASTRO                                     ')
            .AppendLine('  FROM PRODUTO_IMAGEM                                  ')
            .AppendLine('  WHERE CO_PRODUTO = :CO_PRODUTO;                      ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_PRODUTO').AsInteger := co_produto;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
  Result := TObjectList<TImagensModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LStream := TMemoryStream.Create;
      LJpg    := TJPEGImage.Create;
      try
        LImagens             := TImagensModel.Create;
        LImagens.co_imagem   := Self.FQuery.FieldByName('CO_IMAGEM').AsInteger;
        LImagens.co_produto  := Self.FQuery.FieldByName('CO_PRODUTO').AsInteger;
        TBlobField(Self.FQuery.FieldByName('IM_PRODUTO')).SaveToStream(LStream);

        LJpg.LoadFromStream(LStream);
        LStream.Position := 0;

        LImagens.im_produto  := LStream;
        LImagens.tx_extensao := Self.FQuery.FieldByName('TX_EXTENSAO').AsString;
        LImagens.dt_cadastro := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
        Result.Add(LImagens);
        Self.FQuery.Next;
      finally
        FreeAndNil(LJpg);
      end;
    end;
  end;
end;

function TImagensDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TImagensDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TImagensDao.Insert(AImagensModel: TImagensModel): Boolean;
var
  LStream : TMemoryStream;
  LJpg : TJpegImage;
begin
  LStream := TMemoryStream.Create;
  LJpg    := TJpegImage.Create;
  try
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('INSERT INTO PRODUTO_IMAGEM (CO_IMAGEM, CO_PRODUTO, IM_PRODUTO, TX_EXTENSAO)      ')
              .AppendLine('                    VALUES (:CO_IMAGEM, :CO_PRODUTO, :IM_PRODUTO, :TX_EXTENSAO); ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
    Self.FQuery.ParamByName('CO_IMAGEM').AsInteger  := AImagensModel.co_imagem;
    Self.FQuery.ParamByName('CO_PRODUTO').AsInteger := AImagensModel.co_produto;
    Self.FQuery.ParamByName('TX_EXTENSAO').AsString := AImagensModel.tx_extensao;

    LJpg.LoadFromStream(AImagensModel.im_produto);
    LJpg.SaveToStream(LStream);
    LStream.Position := 0;
    Self.FQuery.ParamByName('IM_PRODUTO').LoadFromStream(LStream, ftGraphic);

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
  finally
    FreeAndNil(LJpg);
    FreeAndNil(LStream);
  end;
end;

end.
