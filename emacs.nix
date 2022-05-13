{ pkgs, ... }:
let
  inherit (builtins) fetchTarball;
  happy-mode = (fetchTarball {
    url = "https://github.com/sergv/happy-mode/archive/ac6db0e1c6370c005aaf4bd08702821b16d4c2b6.tar.gz";
    # sha256 = "1cx9yajwcyl9h0n34gh3qsk8vmz8mfwqagc8wq5vpybqk6nm9724";
  });
  genFiles = args:
    if args ? files then
      '':files (${pkgs.lib.strings.concatMapStringsSep " " (x: "\"${x}\"") args.files})''
    else
      "";
  providers = {
    "github" = "https://github.com";
    "gitlab" = "https://gitlab.com";
  };
  fromProvider = p: providers."${p}";
  mkPackage = { name, repo, provider ? "github", sha256, self, ... }@args: self.straightBuild {
    pname = name;
    src = pkgs.fetchgit {
      inherit sha256;
      url = "${fromProvider provider}/${repo}";
    };
    buildPhase = ":";
    installPhase = ''
      LISPDIR=$out/share/emacs/site-lisp
      install -d $LISPDIR
      cp -r * $LISPDIR
    '';
    recipe = pkgs.writeText "recipe" ''
      (${name}
      :repo "${repo}"
      :fetcher github
      ${genFiles(args)})
    '';
  } // args;
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    # new
    url = "https://github.com/vlaci/nix-doom-emacs/archive/33064319607745856f488a998ca3db8ffcede865.tar.gz";
    sha256 = "0h9sg744w6nafmp1lkpdrb71c6gpy8p8i05iy5rn1f3vqdzvd90x";
    # old
    # url = "https://github.com/vlaci/nix-doom-emacs/archive/201e023c671c50c0ee2efe5ca11143161166a736.tar.gz";
    # sha256 = "0a9df4sg9bq13zvd6cgc1qidmzd9lll55fx25d9frm5fl7jrn561";
  }) {
    doomPrivateDir = ./doom.d;
    dependencyOverrides = {
      "emacs-overlay" = (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/92f0648d4001e7921d77c17f17f23df02697937f.tar.gz;
          sha256 = "01jpnr7bm1ch1grzx1515flwbmkzn9jx05kpc8sdzpsy1ix5vska";
      });
    };
    emacsPackagesOverlay = self: super:
      let mkPackage' = args: mkPackage (args // { inherit self; });
      in {
        gitignore-mode = self.git-modes;
        gitconfig-mode = self.git-modes;
        cl-format = mkPackage' {
          provider = "gitlab";
          name = "cl-format";
          repo = "akater/elisp-cl-format";
          sha256 = "10z53j111wvgy0fbnxm3mpc9an75dblvy5zkq9733vjliycbbgv4";
        };
        snippets = self.trivialBuild rec {
          pname = "snippets";
          ename = pname;
          src = ./development/emacs/snippets;
          files = [ "**/*" ];
        };
        org-roam-ui = self.trivialBuild {
          pname = "org-roam-ui";
          ename = "org-roam-ui";
          version = "2020-08-14";
          buildInputs = [ self.f self.simple-httpd self.org-roam self.websocket ];
          src = builtins.fetchTarball {
            url = "https://github.com/org-roam/org-roam-ui/archive/b153f4fee99e36dec0fb56d987026d53bf97a0e8.tar.gz";
          };
        };
        happy-mode = self.trivialBuild {
          pname = "happy-mode";
          ename = "happy-mode";
          version = "1";
          src = happy-mode;
          buildPhase = ":";
        };
        org-html-themify = mkPackage' {
          name = "org-html-themify";
          repo = "DogLooksGood/org-html-themify";
          sha256 = "0hahaw3nd2a4a94vr01lgm6vczxz9pcdv43xfjkyjlmnnd3aclg1";
          files = [
            "*.el"
            "*.js"
            "*.css"
          ];
        };
        hexrgb = mkPackage' {
          name = "hexrgb";
          repo = "emacsmirror/hexrgb";
          sha256 = "0y5l6hrzm5j2jfrm5jp5zrxhxgvf930m2k4nyvk0rllpx0i1271z";
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
    nodePackages.indium

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

    # required for :lang python
    # python-language-server # does not works, hangs
    # python38Packages.python-language-server # marked as broken

    # used in xclip
    expect
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
