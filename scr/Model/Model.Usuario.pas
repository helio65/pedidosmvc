unit Model.Usuario;

interface

uses
  System.Classes;

type
  TUsuarioModel = class
  private
    Fin_ativo: String;
    Fdt_alteracao: TDateTime;
    Ftx_senha: String;
    Fdt_cadastro: TDateTime;
    Fnm_usuario: String;
    Fco_usuario: Integer;
    Ftx_login: String;
    procedure Setco_usuario(const Value: Integer);
    procedure Setdt_alteracao(const Value: TDateTime);
    procedure Setdt_cadastro(const Value: TDateTime);
    procedure Setin_ativo(const Value: String);
    procedure Setnm_usuario(const Value: String);
    procedure Settx_login(const Value: String);
    procedure Settx_senha(const Value: String);
  public
    property co_usuario : Integer read Fco_usuario write Setco_usuario;
    property nm_usuario : String read Fnm_usuario write Setnm_usuario;
    property tx_login : String read Ftx_login write Settx_login;
    property tx_senha : String read Ftx_senha write Settx_senha;
    property in_ativo : String read Fin_ativo write Setin_ativo;
    property dt_cadastro : TDateTime read Fdt_cadastro write Setdt_cadastro;
    property dt_alteracao : TDateTime read Fdt_alteracao write Setdt_alteracao;
  end;

implementation

{ TUsuarioModel }

procedure TUsuarioModel.Setco_usuario(const Value: Integer);
begin
  Fco_usuario := Value;
end;

procedure TUsuarioModel.Setdt_alteracao(const Value: TDateTime);
begin
  Fdt_alteracao := Value;
end;

procedure TUsuarioModel.Setdt_cadastro(const Value: TDateTime);
begin
  Fdt_cadastro := Value;
end;

procedure TUsuarioModel.Setin_ativo(const Value: String);
begin
  Fin_ativo := Value;
end;

procedure TUsuarioModel.Setnm_usuario(const Value: String);
begin
  Fnm_usuario := Value;
end;

procedure TUsuarioModel.Settx_login(const Value: String);
begin
  Ftx_login := Value;
end;

procedure TUsuarioModel.Settx_senha(const Value: String);
begin
  Ftx_senha := Value;
end;

end.
