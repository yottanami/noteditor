set -e

current=$(cd "$(dirname "$0")/." >/dev/null 2>&1 ; pwd -P)
noteditor_home="$current/.."

# Coloring Functions
info() {
    if [ "$1" ]
    then
        echo "[\033[01;32mINFO\033[00m]: $1"
    fi
}

error() {
    if [ "$1" ]
    then
        echo "[\033[01;31mERR\033[00m]: $1"
    fi
}

warn() {
    if [ "$1" ]
    then
        echo "[\033[01;33mWARN\033[00m]: $1"
    fi
}

install_runners() {
    info "Creating the runner scripts..."

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sed -i "s'___NOTEDITOR_HOME___'$noteditor_home'" "$noteditor_home/noteditor"
        sed -i "s'___NOTEDITOR_HOME___'$noteditor_home'" "$noteditor_home/noteditor-wm"
    fi

    if [[ "$OSTYPE" == "darwin"* ]]; then
        sed -i'' -e "s'___NOTEDITOR_HOME___'$noteditor_home'" "$noteditor_home/noteditor"
        sed -i'' -e "s'___NOTEDITOR_HOME___'$noteditor_home'" "$noteditor_home/noteditor-wm"
    fi

    chmod +x "$noteditor_home/noteditor"
    chmod +x "$noteditor_home/noteditor-wm"

#    info "Copying conifg file to ~/.noteditor.el..."
#    cp "$noteditor_home/config/noteditor.user.el" ~/.noteditor.el

    info "Installing the runners..."

    sudo mkdir -p /usr/local/bin/
    sudo rm -f /usr/local/bin/noteditor
    sudo rm -f /usr/local/bin/noteditor-wm
    sudo ln -s "$noteditor_home/noteditor" /usr/local/bin/noteditor
    sudo ln -s "$noteditor_home/noteditor-wm" /usr/local/bin/noteditor-wm

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        info "Copying share files..."
        sudo mkdir -p /usr/share/noteditor/
        sudo mkdir -p /usr/local/share/applications
        sudo cp "$noteditor_home/share/applications/noteditor.desktop" /usr/local/share/applications
        sudo cp -r "$noteditor_home/share/*" /usr/share/noteditor/
        sudo mkdir -p /usr/share/xsessions/
        sudo cp -r "$noteditor_home/share/xsessions/noteditor.desktop" /usr/share/xsessions/
    else
        info "Skipping share files since this is not a Linux env..."
    fi
}

install_extras() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        info "Copying share files..."
        sudo mkdir -p /usr/share/noteditor/
        sudo cp "$noteditor_home/share/applications/noteditor.desktop" /usr/local/share/applications
        sudo cp -r "$noteditor_home/share/*" /usr/share/noteditor/
        sudo cp -r "$noteditor_home/share/xsessions/noteditor.desktop" /usr/share/xsessions/
    else
        info "Skipping share files since this is not a Linux env..."
    fi

}

eval "install_$1"
