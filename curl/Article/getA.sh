#!/bin/bash

#-c? - credentials
#-s? - created since
#-u? - created until           date format: YYYY-MM-DD
#-a? - created at
#-n? - author name is
#-y? - category id is
#-t? - title contains
#-e? - content contains
#-h? - search in content, author name, category name
#-o? - order/sort by "date;author;category;image_number"
#-l? - limit
#-f? - offset

#example: curl/Article/getA.sh -u 2022-06-08 -o image_number -y1 -t Как

while getopts ":c:s:u:a:n:y:t:e:h:o:l:f:" opt; do
  case $opt in
    c) cred="$OPTARG"
    ;;
    s) created_since="$OPTARG"
    ;;
    u) created_until="$OPTARG"
    ;;
    a) created_at="$OPTARG"
    ;;
    n) author="$OPTARG"
    ;;
    y) category_id="$OPTARG"
    ;;
    t) title_has="$OPTARG"
    ;;
    e) content_has="$OPTARG"
    ;;
    h) search="$OPTARG"
    ;;
    o) sort_by="$OPTARG"
    ;;
    l) limit="$OPTARG"
    ;;
    f) offset="$OPTARG"
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    exit 1
    ;;
  esac

  case $OPTARG in
    -*) echo "Option $opt needs a valid argument"
    exit 1
    ;;
  esac
done

if [ ! -z "${created_since}" ] 
then 
    created_sinceQuery=(--data-urlencode "created_since=$created_since")
else
    created_sinceQuery=()
fi

if [ ! -z "${created_until}" ] 
then 
    created_untilQuery=(--data-urlencode "created_until=$created_until")
else
    created_untilQuery=()
fi

if [ ! -z "${created_at}" ] 
then 
    created_atQuery=(--data-urlencode "created_at=$created_at")
else
    created_atQuery=()
fi

if [ ! -z "${author}" ] 
then 
    authorQuery=(--data-urlencode "author=$author")
else
    authorQuery=()
fi

if [ ! -z "${category_id}" ] 
then 
    category_idQuery=(--data-urlencode "category_id=$category_id")
else
    category_idQuery=()
fi

if [ ! -z "${title_has}" ] 
then 
    title_hasQuery=(--data-urlencode "title_has=$title_has")
else
    title_hasQuery=()
fi

if [ ! -z "${content_has}" ] 
then 
    content_hasQuery=(--data-urlencode "content_has=$content_has")
else
    content_hasQuery=()
fi

if [ ! -z "${search}" ] 
then 
    searchQuery=(--data-urlencode "search=$search")
else
    searchQuery=()
fi

if [ ! -z "${sort_by}" ] 
then 
    sort_byQuery=(--data-urlencode "sort_by=$sort_by")
else
    sort_byQuery=()
fi

if [ ! -z "${limit}" ] 
then 
    limitQuery=(--data-urlencode "limit=$limit")
else
    limitQuery=()
fi

if [ ! -z "${offset}" ] 
then 
    offsetQuery=(--data-urlencode "offset=$offset")
else
    offsetQuery=()
fi

curl -G \
  --header "Authorization: Basic `echo -n $cred | base64`" \
  --url http://localhost:3000/article/get \
  "${limitQuery[@]}" \
  "${authorQuery[@]}" \
  "${offsetQuery[@]}" \
  "${searchQuery[@]}" \
  "${sort_byQuery[@]}" \
  "${title_hasQuery[@]}" \
  "${created_atQuery[@]}" \
  "${category_idQuery[@]}" \
  "${content_hasQuery[@]}" \
  "${created_untilQuery[@]}" \
  "${created_sinceQuery[@]}" 