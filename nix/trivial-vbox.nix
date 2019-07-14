let
  vbox = { deployment.targetEnv = "virtualbox"; };
in
{ proxy    = vbox;
  backend1 = vbox;
  backend2 = vbox;
}
