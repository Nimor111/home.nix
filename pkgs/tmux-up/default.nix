{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "tmux-up";
  version = "87a89989ad4a588eb5f02cc2cda50335d26d8435";

  src = fetchFromGitHub {
    owner = "jamesottaway";
    repo = "tmux-up";
    rev = version;
    sha256 = "1ykxqayz7qh7c7xzz01wkcvw4hmxg1nr50a6lpr0w0w99qcz0y8x";
  };

  dontBuild = true;

  installPhase = ''
    install -Dm755 -t $out/bin tmux-up
  '';
}
