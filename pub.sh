REL_EBIN_DIR="/Users/roowe/myproject/hongkongresort-el-ebin"
DB_MATCH=p04
ebin() {
    rebar clean
    rebar com -j3

    for dir in `ls ./deps/`; do
        mkdir -p $REL_EBIN_DIR/deps/$dir/
        cp -rf deps/$dir/ebin/ $REL_EBIN_DIR/deps/$dir/
        [ -d "deps/$dir/priv" ] && cp -rf deps/$dir/priv $REL_EBIN_DIR/deps/$dir/
    done

    mkdir -p $REL_EBIN_DIR/ebin/
    cp -rf ./ebin $REL_EBIN_DIR/

    mkdir -p $REL_EBIN_DIR/sh/
    cp -rf ./sh/appctl $REL_EBIN_DIR/sh/

}

db() {
    ./sh/db_compare
    mkdir -p $REL_EBIN_DIR/db_update/
    cd $REL_EBIN_DIR/db_update
    for file in *.sql
    do
        echo $file
        git rm $file
    done
    mkdir -p $REL_EBIN_DIR/db_update/
    new=0
    for file in /tmp/mysql_compare/$DB_MATCH*.sql
    do
        if [ -f $file ]
        then
            new=1
            break
        fi 
    done
    if [ $new == "1" ]
    then
        cp /tmp/mysql_compare/$DB_MATCH*  $REL_EBIN_DIR/db_update/
    fi
}

git_push() {
    cd $REL_EBIN_DIR
    find deps -iname ".gitignore" -exec rm {} \;
    git pull origin master
    git add .
    git commit -m "auto pub"
    git push origin master
}
help()
{
    echo "./$0 all"  
    echo "./$0 ebin"
}
case $1 in
    'all')
        ebin 
        db
        git_push;;
    'ebin')
        ebin
        git_push;;
    'db') 
        db;;
    *)
        help;;
esac
