set -e
bookid=`uuid`
if [ ! -f /var/mobile/Media/Books/${bookid}.pdf ];
then
	cp $1.pdf /var/mobile/Media/Books/${bookid}.pdf
	addstring="{\"Extension\":\"pdf\",\"Has Artwork\":true,\"Is Protected\":false,\"Kind\":\"unknown\",\"MIME Type\" : \"application/pdf\", \"Name\":\"$1\",\"Path\":\"${bookid}.pdf\",\"Persistent ID\":\"$bookid\"}"
	plutil -Books -arrayadd -value "$addstring" -type json /var/mobile/Media/Books/Books.plist
else
	echo "Failed to create file, ${bookid}.pdf already exists, or an error occurred."
fi
