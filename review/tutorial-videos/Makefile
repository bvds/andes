#
#   The videos were made with QuickTime on OS X, default
#   settings, except with the internal microphone turned off.
#
# QuickTime produces H.264 codec quicktime vidoes (*.mov).
# QuickTime does export into other formats, but they are all H.264.
# For cross browser compatability (as of Feb. 2012), one must
# provide H.264 and either Theora or VP8 (webm) codecs.
# 
# Aim for  
#         H.264 codec with mpeg 4  (*.mp4)
#         theora codec with ogg    (*.ogv)
#
# Full details at:
# http://wiki.videolan.org/Documentation:Streaming_HowTo/Advanced_Streaming_Using_the_Command_Line
#
mp4 := $(patsubst %.mov,%.mp4,$(wildcard *.mov))
ogv := $(patsubst %.mov,%.ogv,$(wildcard *.mov))

convert:  $(ogv) $(mp4)
convert-ogv: $(ogv)
convert-mp4: $(mp4)

#
#   I can't get this to work:  the output videos don't work in IE 9
#   and I can't see why output is different from conversion via the UI.
#   See http://forum.videolan.org/viewtopic.php?f=2&t=98465
#
#  deinterlace=0 ,samplerate=44100
%.mp4: %.mov
	cvlc -v "$<" --sout="#transcode{vcodec=h264,fps=20,scale=1,acodec=mp4a,channels=2,ab=128}:standard{access=file,mux=ts,dst='$@'}" vlc://quit

#
%.ogv: %.mov
	cvlc -v "$<" --sout="#transcode{vcodec=theo,fps=20,scale=1,acodec=vorb,channels=2,ab=128}:standard{access=file,mux=ogg,dst='$@'}" vlc://quit