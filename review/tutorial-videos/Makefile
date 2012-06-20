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
#   To see what the UI conversion is, open the Messages window
#   and set verbosity level to 1 or higher.  The transcode settings
#   are shown there. 
#
#   Sadly, it looks like transcode cannot adjust audio volume.
#
%.mp4: %.mov
	cvlc -v "$<" --sout="#transcode{vcodec=h264,vb=0,scale=0,acodec=mp4a,ab=128,channels=2,samplerate=44100}:file{dst='$@'}" vlc://quit

#
%.ogv: %.mov
	cvlc -v "$<" --sout="#transcode{vcodec=theo,vb=800,scale=1,acodec=vorb,ab=128,channels=2,samplerate=44100}:file{dst='$@'}" vlc://quit
