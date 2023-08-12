#! /bin/sh

. tests/common.sh

export TEGFS_ROOT=dist/recreateroot

rm -rf "$TEGFS_ROOT"

sh tests/make-example-config.sh
sh tests/make-example-categorization.sh
sh tests/make-example-rules.sh

t_tegfs add \
    --title "Lambda Calculus paper" \
    --content "dist/dbfiles/church.pdf" \
    --tag science --tag document \
    --date "2022-12-26T13:33:25+0000"


t_tegfs add \
    --title "Photo of a guy" \
    --content "dist/dbfiles/pexels-artūras-kokorevas-14795847.jpg" \
    --tag "with=X,$" --tag man=X --tag photo --tag object=X \
    --date "2022-12-26T13:36:07+0000"


t_tegfs add \
    --title "Woman in greece" \
    --tag "with=X,$" --tag woman=X --tag photo --tag object=X \
    --mimetype "tegfs/entry" \
    --date "2022-12-26T13:36:31+0000"


t_tegfs add \
    --title "Photo of a city" \
    --content "dist/dbfiles/pexels-céline-14771128.jpg" \
    --tag photo \
    --date "2022-12-26T13:36:44+0000"


t_tegfs add \
    --title "Bread" \
    --content "dist/dbfiles/pexels-geraud-pfeiffer-6608542.jpg" \
    --tag photo \
    --date "2022-12-26T13:36:58+0000"


t_tegfs add \
    --title "Photo of a city" \
    --content "dist/dbfiles/pexels-hatice-baran-14770663.jpg" \
    --tag photo \
    --date "2022-12-26T13:37:07+0000"


t_tegfs add \
    --title "Colorful art" \
    --content "dist/dbfiles/pexels-steve-johnson-1548110.jpg" \
    --tag drawing --tag art \
    --date "2022-12-26T13:37:25+0000"


t_tegfs add \
    --title "Football commercial" \
    --content "dist/dbfiles/FischersBrea.mp4" \
    --tag video \
    --source "https://ia800203.us.archive.org/31/items/FischersBrea/FischersBrea.mp4" \
    --date "2022-12-26T13:38:28+0000"


t_tegfs add \
    --title "carpet commercial" \
    --content "dist/dbfiles/allen_carpet.mp4" \
    --tag video \
    --source "https://ia600301.us.archive.org/23/items/allen_carpet/allen_carpet.mp4" \
    --date "2022-12-26T13:38:54+0000"


t_tegfs add \
    --title "Toothpaste commercial" \
    --content "dist/dbfiles/macleanstoot.mp4" \
    --tag video \
    --source "https://ia800205.us.archive.org/6/items/macleanstoot/macleanstoot.mp4" \
    --date "2022-12-26T13:39:41+0000"


t_tegfs add \
    --title "Collection of wallpapers" \
    --content "dist/dbfiles/wallpapers" \
    --tag "collection-of=X,$" --tag photo=X --tag image=X \
    --dirpreview \
    --date "2022-12-26T13:45:01+0000"


t_tegfs add \
    --title "Soundie - [Hawaiian Hula Song]" \
    --content "dist/dbfiles/soundie_5.mp3" \
    --tag song \
    --source "https://ia800301.us.archive.org/12/items/soundie_5/soundie_5.mp3" \
    --date "2022-12-26T13:47:35+0000"


t_tegfs add \
    --title "USA's DOD nuclear war ASMR" \
    --content "dist/dbfiles/0771_Duck_and_Cover_12_33_20_12.mp3" \
    --tag audio --tag recording \
    --source "https://ia800402.us.archive.org/8/items/0771_Duck_and_Cover_12_33_20_12/0771_Duck_and_Cover_12_33_20_12.mp3" \
    --date "2022-12-26T13:50:51+0000"


t_tegfs add \
    --title "Once in a while" \
    --content "dist/dbfiles/SoundieP.mp3" \
    --tag song \
    --source "https://ia800300.us.archive.org/30/items/SoundieP/SoundieP.mp3" \
    --date "2022-12-26T13:52:19+0000"


t_tegfs add \
    --title "vau.place website" \
    --content "dist/dbfiles/vauplace" \
    --tag document \
    --source "https://vau.place" \
    --date "2022-12-26T13:54:27+0000"


t_tegfs add \
    --title "Prelinger public archive" \
    --content "https://archive.org/details/prelinger" \
    --no-download \
    --tag document --tag website \
    --date "2022-12-26T13:56:25+0000"


t_tegfs add \
    --title "Lecture 1A: Overview and Introduction to Lisp" \
    --content "https://youtube.com/watch?v=-J_xL4IGhJA" \
    --no-download \
    --tag lecture \
    --date "2022-12-26T13:58:38+0000"


t_tegfs add \
    --title "Eneida by Kotlarevskiy" \
    --content "dist/dbfiles/eneida.txt" \
    --tag book \
    --date "2023-07-20T18:17:02+0000"


t_tegfs add \
    --title "Shall I compare thee to a summer’s day?" \
    --content "dist/dbfiles/shake1.txt" \
    --tag pasta \
    --date "2023-07-20T18:40:47+0000"

