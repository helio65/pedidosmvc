unit Controller.Imagens;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, System.StrUtils, Framework.Factory, Model.Imagens,
  View.Imagens, FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao, Vcl.Dialogs,
  System.Variants, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Graphics, Data.DB,
  Vcl.Imaging.jpeg;

type
  TImagensController = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FView : TFrmImagem;
    FFactory : TFactory;
    FStatus : TStatus;
    FCo_Produto : Integer;
    FImagensModel : TImagensModel;
    FOpenPictureDialog : TOpenPictureDialog;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    function Delete(co_imagem : Integer) : Boolean;
    procedure Save(aObject : TImagensModel);
    function GetAll(co_produto : Integer) : TObjectList<TImagensModel>;
    constructor Create(co_produto : Integer);
    destructor Destroy; override;
  end;

implementation

{ TImagensController }

procedure TImagensController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão desta Imagem?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.Delete(Self.FView.FDMImagensCO_IMAGEM.AsInteger) then
      Self.FormShow(Sender);
  end;
end;

procedure TImagensController.btnCancelarClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TImagensController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TImagensController.btnGravarClick(Sender: TObject);
var
  LStream : TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    TBlobField(Self.FView.FDMImagensIM_PRODUTO).SaveToStream(LStream);
    LStream.Position := 0;
    Self.FImagensModel.co_imagem   := Self.FView.FDMImagensCO_IMAGEM.AsInteger;
    Self.FImagensModel.co_produto  := Self.FView.FDMImagensCO_PRODUTO.AsInteger;
    Self.FImagensModel.im_produto  := LStream;
    Self.FImagensModel.tx_extensao := Self.FView.FDMImagensTX_EXTENSAO.AsString;
    Self.Save(Self.FImagensModel);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TImagensController.btnNovoClick(Sender: TObject);
var
  LStream : TMemoryStream;
  LJpg : TJpegImage;
begin
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  if Self.FOpenPictureDialog.Execute then
  begin
    if not String(Self.FOpenPictureDialog.FileName).IsEmpty then
    begin
      LStream := TMemoryStream.Create;
      LJpg    := TJpegImage.Create;
      try
        if not Self.FView.FDMImagens.Active then
          Self.FView.FDMImagens.Open;
        Self.FView.FDMImagens.Insert;
        Self.FView.FDMImagensCO_IMAGEM.AsInteger := Self.FFactory.GetNextID('INC_CO_IMAGEM', DMConexao.Conexao);
        Self.FView.FDMImagensCO_PRODUTO.AsInteger := Self.FCo_Produto;

        LJpg.LoadFromFile(Self.FOpenPictureDialog.FileName);
        LJpg.SaveToStream(LStream);
        LStream.Position := 0;
        Self.FView.FDMImagensIM_PRODUTO.LoadFromStream(LStream);
        Self.FView.FDMImagensTX_EXTENSAO.AsString := ExtractFileExt(Self.FOpenPictureDialog.FileName).Replace('.', '').Trim;
        Self.FView.FDMImagens.Post;
      finally
        FreeAndNil(LJpg);
        FreeAndNil(LStream);
      end;
    end;
  end;
end;

constructor TImagensController.Create(co_produto : Integer);
begin
  inherited Create;
  Self.FStrB                     := TStringBuilder.Create;
  Self.FQuery                    := TFDQuery.Create(nil);
  Self.FQuery.Connection         := DMConexao.Conexao;
  Self.FView                     := TFrmImagem.Create(nil);
  Self.FView.KeyPreview          := True;
  Self.FView.btnFechar.OnClick   := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick     := Self.btnNovoClick;
  Self.FView.btnGravar.OnClick   := Self.btnGravarClick;
  Self.FView.OnShow              := Self.FormShow;
  Self.FView.OnKeyPress          := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick   := Self.btnApagarClick;
  Self.FFactory                  := TFactory.Create;
  Self.FImagensModel             := TImagensModel.Create;
  Self.FCo_Produto               := co_produto;
  Self.FOpenPictureDialog        := TOpenPictureDialog.Create(nil);
  Self.FOpenPictureDialog.Filter := 'jpg|*.jpg|png|*.png|bmp|*.bmp';
  Self.FView.ShowModal;
end;

function TImagensController.Delete(co_imagem: Integer): Boolean;
begin
  Self.FStatus := TStatus.dsDelete;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
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
    Result := True;
  except
    on e : exception do
    begin
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

destructor TImagensController.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FImagensModel);
  FreeAndNil(Self.FOpenPictureDialog);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TImagensController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TImagensController.FormShow(Sender: TObject);
var
  LImagens : TObjectList<TImagensModel>;
  aIndex: Integer;
  LStream : TMemoryStream;
  LJpg : TJpegImage;
begin
  LImagens := Self.GetAll(Self.FCo_Produto);;
  try
    if LImagens.Count > 0 then
    begin
      Self.FView.FDMImagens.Open;
      Self.FView.FDMImagens.EmptyDataSet;
      for aIndex := 0 to Pred(LImagens.Count) do
      begin
        LStream := TMemoryStream.Create;
        LJpg    := TJpegImage.Create;
        try
          Self.FView.FDMImagens.Insert;
          Self.FView.FDMImagensCO_IMAGEM.AsInteger    := LImagens[aIndex].co_imagem;
          Self.FView.FDMImagensCO_PRODUTO.AsInteger   := LImagens[aIndex].co_produto;

          LJpg.LoadFromStream(LImagens[aIndex].im_produto);
          LJpg.SaveToStream(LStream);

          LStream.Position := 0;
          Self.FView.FDMImagensIM_PRODUTO.LoadFromStream(LStream);
          Self.FView.FDMImagensTX_EXTENSAO.AsString   := LImagens[aIndex].tx_extensao;
          Self.FView.FDMImagensDT_CADASTRO.AsDateTime := LImagens[aIndex].dt_cadastro;
          Self.FView.FDMImagens.Post;
        finally
          FreeAndNil(LJpg);
          FreeAndNil(LStream);
        end;
      end;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMImagens.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);

  finally
    for aIndex := 0 to Pred(LImagens.Count) do
    begin //Removendo os objeto TStrem da lista, senão ocorre vazamento de memória
      var LStreamD : TStream;
      LStreamD := LImagens[aIndex].im_produto;
      FreeAndNil(LStreamD);
    end;
    FreeAndNil(LImagens);
  end;
end;

function TImagensController.GetAll(co_produto : Integer): TObjectList<TImagensModel>;
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

procedure TImagensController.Save(aObject: TImagensModel);
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
    Self.FQuery.ParamByName('CO_IMAGEM').AsInteger  := aObject.co_imagem;
    Self.FQuery.ParamByName('CO_PRODUTO').AsInteger := aObject.co_produto;
    Self.FQuery.ParamByName('TX_EXTENSAO').AsString := aObject.tx_extensao;

    LJpg.LoadFromStream(aObject.im_produto);
    LJpg.SaveToStream(LStream);
    LStream.Position := 0;
    Self.FQuery.ParamByName('IM_PRODUTO').LoadFromStream(LStream, ftGraphic);

    if not Self.FQuery.Connection.InTransaction then
      Self.FQuery.Connection.StartTransaction;
    try
      Self.FQuery.ExecSQL;
      Self.FQuery.Connection.Commit;
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
