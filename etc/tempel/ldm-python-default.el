((python-mode
  .
  ((projectile-project-compilation-cmd
    . "pip install -e .[dev,test] && pre-commit install")
   (projectile-project-configure-cmd
    . "pyenv virtualenv 3.12.2 $(basename $PWD) && pyenv local $(basename $PWD)")
   (projectile-project-run-cmd . "python -m mypackage ")
   (projectile-project-test-cmd . "pytest"))
  ))
