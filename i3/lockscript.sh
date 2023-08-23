img=/tmp/i3lock.png

sudo nice -n -20 sudo -u $USER scrot -o $img
sudo nice -n -20 sudo -u $USER convert $img -scale 10% -scale 1000% $img

sudo nice -n -20 sudo -u $USER i3lock -i $img 
