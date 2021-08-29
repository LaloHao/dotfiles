{ pkgs, ... }:
let
  happy-mode = (builtins.fetchTarball {
    url = "https://github.com/sergv/happy-mode/archive/ac6db0e1c6370c005aaf4bd08702821b16d4c2b6.tar.gz";
    # sha256 = "1cx9yajwcyl9h0n34gh3qsk8vmz8mfwqagc8wq5vpybqk6nm9724";
  });
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = "https://github.com/vlaci/nix-doom-emacs/archive/201e023c671c50c0ee2efe5ca11143161166a736.tar.gz";
    sha256 = "0a9df4sg9bq13zvd6cgc1qidmzd9lll55fx25d9frm5fl7jrn561";
  }) {
    doomPrivateDir = ./doom.d;
    emacsPackagesOverlay = self: super: {
      happy-mode = self.trivialBuild {
        pname = "happy-mode";
        ename = "happy-mode";
        version = "1";
        src = happy-mode;
        buildPhase = ":";
      };
    };
  };
  inherit (import ./packages/dictionaries { }) es_MX;
in {
  # Allow searching for fonts
  fonts.fontconfig.enable = true;
  home.packages = (with pkgs; [
    emacs-all-the-icons-fonts
    ispell

    # Required for :lang org
    nodePackages.typescript
    # nodePackages.tslab

    # Required for :lang sh
    bashdb                # Enables debugging for bash scripts
    nodePackages.bash-language-server  # Enables LSP support (with +lsp flag)
    # with :tools debugger
    shellcheck            # Enables advanced shell script linting
    # zshdb                 # Enables debugging for bash scripts

    # Required for :checkers spell
    # aspell
    (aspellWithDicts (dictionaries: with dictionaries; [
      en                         # english
        en-computers
        en-science
      es                         # spanish
    ]))
    # hunspell
    (hunspellWithDicts [
      hunspellDicts.en_US                      # english
                                 # also see: en_US-large
      es_MX
    ])
    # enchant
    # enchant # can't use multidict

    # Required for :checkers grammar
    languagetool
  ] ++ [
    doom-emacs
  ]);
  home.file.".emacs.d/init.el".text = ''
    (load "default.el")
  '';
  services.emacs = {
    enable = true;
    package = doom-emacs;
    client.enable = true;
  };
}
