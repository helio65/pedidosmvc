unit Model.Empresa;

interface

uses
  System.Classes;
  //https://publica.cnpj.ws/cnpj/24813687000142
type
  TEmpresaModel = class
  private
    Fnm_razao_social: String;
    Ftx_email: String;
    Ftd_alteracao: TDatetime;
    Fco_empresa: Integer;
    Fdt_cadastro: TDatetime;
    Fnu_cnpj: String;
    procedure Setco_empresa(const Value: Integer);
    procedure Setdt_cadastro(const Value: TDatetime);
    procedure Setnm_razao_social(const Value: String);
    procedure Setnu_cnpj(const Value: String);
    procedure Settd_alteracao(const Value: TDatetime);
    procedure Settx_email(const Value: String);
  public
    property co_empresa : Integer read Fco_empresa write Setco_empresa;
    property nm_razao_social : String read Fnm_razao_social write Setnm_razao_social;
    property nu_cnpj : String read Fnu_cnpj write Setnu_cnpj;
    property tx_email : String read Ftx_email write Settx_email;
    property dt_cadastro : TDatetime read Fdt_cadastro write Setdt_cadastro;
    property dt_alteracao : TDatetime read Ftd_alteracao write Settd_alteracao;
  end;

implementation

{ TEmpresaModel }

procedure TEmpresaModel.Setco_empresa(const Value: Integer);
begin
  Fco_empresa := Value;
end;

procedure TEmpresaModel.Setdt_cadastro(const Value: TDatetime);
begin
  Fdt_cadastro := Value;
end;

procedure TEmpresaModel.Setnm_razao_social(const Value: String);
begin
  Fnm_razao_social := Value;
end;

procedure TEmpresaModel.Setnu_cnpj(const Value: String);
begin
  Fnu_cnpj := Value;
end;

procedure TEmpresaModel.Settd_alteracao(const Value: TDatetime);
begin
  Ftd_alteracao := Value;
end;

procedure TEmpresaModel.Settx_email(const Value: String);
begin
  Ftx_email := Value;
end;

end.
