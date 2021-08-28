FROM ubuntu:20.04

RUN \
    apt-get update \
 && apt-get install -y software-properties-common \
 && add-apt-repository ppa:deadsnakes/ppa \
 && apt-get update \
 && apt-get install -y \
    curl \
    git \
    python3.9 \
    python3.9-distutils \
    python3.9-venv \
    stow \
    vim

RUN useradd --create-home user

USER user
WORKDIR /home/user

RUN \
    curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py \
 && python3.9 get-pip.py \
 && rm get-pip.py

RUN \
    python3.9 -m pip install --user pipx \
 && ~/.local/bin/pipx install nox

COPY --chown=user:user . /home/user/dotfiles

RUN \
    mkdir -p ~/.dotfiles.d/ \
 && ln -s ~/dotfiles ~/.dotfiles.d/main

# Remove default files, otherwise stow fails.
RUN rm ~/.bashrc ~/.profile

RUN \
    cd dotfiles \
 && ~/.local/bin/nox -e all -- install

# Use login shell so .profile gets sourced.
CMD ["bash", "-l"]
