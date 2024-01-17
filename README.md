# Pulsar

This is a small Emacs package that temporarily highlights the current
line after a given function is invoked. The affected functions are
defined in the user option `pulsar-pulse-functions`. What Pulsar does
is set up an advice so that those functions run a hook after they are
called. The pulse effect is added there (`pulsar-after-function-hook`).

+ Package name (GNU ELPA): `pulsar`
+ Official manual: <https://protesilaos.com/emacs/pulsar>
+ Change log: <https://protesilaos.com/emacs/pulsar-changelog>
+ Git repositories:
  + GitHub: <https://github.com/protesilaos/pulsar>
  + GitLab: <https://gitlab.com/protesilaos/pulsar>
+ Backronym: Pulsar Unquestionably Luminates, Strictly Absent the Radiation.
