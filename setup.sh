# need to setup ssh first to copy the config repository
common_minimal_packages='git make curl'
common_extra_packages='htop thunderbird firefox neovim kitty zsh gimp keepassxc zathura vlc telegram-desktop'
manual_minimal_packages='code'
manual_extra_packages='nextcloud-client i3-wm exa mattermost slack-desktop steam xournal'
conda_path=~/.local/anaconda3
conda_link=https://repo.anaconda.com/archive/Anaconda3-2021.05-Linux-x86_64.sh
# why would i define this twice?
tex_path=~/.local/texlive
TEXLIVE_INSTALL_PREFIX=~/.local/texlive
config_url=git@github.com:LukasNickel/cfg.git

# not needed anymore?
#declare -A osInfo;
#osInfo[/etc/debian_version]=apt-get
#osInfo[/etc/arch-release]=pacman
#osInfo[/etc/redhat-release]=yum
#osInfo[/etc/fedora-release]=dnf
#osInfo[/etc/gentoo-release]=emerge
#osInfo[/etc/SuSE-release]=zypper

#for f in ${!osInfo[@]}
#do
#    if [[ -f $f ]];then
#        echo Detected package manager: ${osInfo[$f]}
#    fi
#done
ToDo() {
    echo "Install the manual packages"
    echo "Login to stuff (messenger, thunderbird, firefox, ...)"
}

install_conda() {
    echo "Searching for conda under $conda_path"
    if [ ! -d $conda_path ]; then
        echo "Conda not found. Installing anaconda using the installer from $conda_link"
        wget $conda_link -O conda_installer.sh
        bash conda_installer.sh -p $conda_path -b
        source $conda_path/bin/activate
        conda init
        source ~/.bashrc
        echo "Installing mamba via conda. Dont freak out if this takes some time"
        conda install -c conda-forge --yes mamba
        mamba install -c conda-forge --yes uncertainties
    else
        echo "Anaconda is installed already, skipping this step."
    fi
}

install_texlive() {
    echo "Searching for tex under $tex_path"
    if [ ! -d $tex_path ]; then
        echo "TeXLive not found."
        read -p "Do you wish to install TeXLive now? Warning: This takes a while because there is a lot of data to download (y/N)" answer
        case ${answer:0:1} in
            y|Y|yes|Yes|Ja|ja )
                cd ~/.local
                curl -L http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz | tar xz
                env TEXLIVE_INSTALL_PREFIX=~/.local/texlive ./install-tl-*/install-tl
                source ~/.bashrc
                tlmgr option autobackup -- -1
                tlmgr option repository http://mirror.ctan.org/systems/texlive/tlnet
                tlmgr update --self --all --reinstall-forcibly-removed
                echo 'export PATH="$HOME/.local/texlive/2021/bin/x86_64-linux:$PATH"' >> ~/.bashrc
                ;;
            * )
                echo "Skipping the TeXLive installation. You can call this script again at a later stage to continue."
                ;;
        esac
    else
        echo "TeXLive is installed already, skipping the installation"
    fi

}

add_tex_themes() {
    git clone https://github.com/maxnoe/tudobeamertheme $(kpsewhich --var-value TEXMFHOME)/tex/latex/tudobeamertheme
    git clone https://github.com/maxnoe/tudothesis $(kpsewhich --var-value TEXMFHOME)/tex/latex/tudothesis
}

configure_git() {
    echo "Setting your git settings"
    echo "Whats your name?"
    read name
    git config --global user.name "$name"
    echo "Whats your email adress?"
    read mail
    git config --global user.email "$mail"
    git config --global rebase.stat true
    git config --global merge.conflictstyle diff3
    # configurable?
    git config --global core.editor "codium --wait"
}

setup_configs() {
    # details:
    # https://www.atlassian.com/git/tutorials/dotfiles
    echo ".cfg" >> .gitignore
    git clone --bare $config_url $HOME/.cfg
    function config {
       /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
    }
    mkdir -p .config-backup
    config checkout
    if [ $? = 0 ]; then
      echo "Checked out config.";
      else
        echo "Backing up pre-existing dot files.";
        config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
    fi;
    config checkout
    config config status.showUntrackedFiles no
    # more minow things
    pip install neovim
    chsh -s /usr/bin/zsh
    nvim +PluginInstall +qall
    echo "The conda installation might be messed up :/"
}

install_packages(){
if [ -x "$(command -v apt-get)" ]
then
    sudo apt-get install --assume-yes $1
elif [ -x "$(command -v pacman)" ]
then
    sudo pacman -S --noconfirm  $1
elif [ -x "$(command -v yum)" ]
then
    sudo yum install -y $1
elif [ -x "$(command -v dnf)" ]
then
    sudo dnf install -y $1
elif [ -x "$(command -v emerge)" ]
then
    sudo emerge $1
elif [ -x "$(command -v zypper)" ]
then
    sudo zypper install -y $1
else
    echo "FAILED TO INSTALL PACKAGE: Package manager not found. You must manually install: $1">&2;
fi
}

install_manually() {
    echo "WARNING: You will need to install $1 manually. This is not implemented yet"
    echo "The package(s) might not be available for all distributions or named differently"
}

echo "This script is meant to collect some common setup steps. You can perform the steps individually or all at once  \n"
echo "- Best exectute this from the home folder, there might be some path assumptions left. I try to avoid it, but you know..."
echo "- You will need a working internet connection and some steps will take a while, especially conda and TeXLive"
echo "- Mamba will be installed in addition to conda to speed up the enviroment solving in the future. This is slow, dont panic"
echo "- VSCode is not directly available in most distributions. On Arch its there, but I still skip the installation for now."
echo "- The relevant toolbox settings end up in your ~/.bashrc. If you use a different shell, you need to copy them manually."
echo""


echo "What shall we do with our precious time?"
echo "
1: Toolbox workshop setup
2: Packages only
3: Setup git
4: Conda only (slow)
5: TeXLive only (slooooooow)
6: Add LaTeX-themes (only tested in combination with 4)
7: My complete setup (Remember to copy your ssh keys and passwords first, future me. Equal to 9->4->5->6->8)
8: My configs only (-> 7)
9: My packages only (-> 7, remember to install the missing packages by hand!)
"

read -p "Select the step to perform:" answer
case ${answer:0:1} in
    1)
        echo "Starting ..."
        ;;
    2)
        install_packages $common_minimal_packages
        install_manually $manual_minimal_packages
        ;;
    3)  
        configure_git
        ;;
    4)
        install_conda
        ;;
    5)  
        install_texlive
        ;;
    6)  
        add_tex_themes
        ;;
    7)  
        install_packages $common_minimal_packages
        install_packages $common_extra_packages
        install_manually $manual_minimal_packages
        install_manually $manual_extra_packages
        install_conda
        install_texlive
        add_tex_themes
        setup_configs
        ;;
    8)  
        setup_configs
        ;;
    9)
        install_packages $common_minimal_packages
        install_packages $common_extra_packages
        install_manually $manual_minimal_packages
        install_manually $manual_extra_packages
        ;;       
    *)
        exit 1
        ;;
esac
