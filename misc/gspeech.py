#!/usr/bin/python

import sys		#for cmd line argv
import time		#for delay
import pygst		#for playing mp3 stream
import gst		# " "
import urllib2

def speack(phrase):

    #tts_string = '+'.join(words)    
    tts_string = phrase.replace(" ", "+")

    music_stream_uri = 'http://translate.google.com/translate_tts?tl=en&q=' + tts_string
    player = gst.element_factory_make("playbin", "player")
    print music_stream_uri
    player.set_property('uri', music_stream_uri)
    player.set_state(gst.STATE_PLAYING)


if __name__ == '__main__':
    
    url = "https://www.google.com/speech-api/v1/recognize?xjerr=1&client=chromium&lang=en-US"
    flac=open("test1.flac","rb").read()
    header = {'Content-Type' : 'audio/x-flac; rate=16000'}
    req = urllib2.Request(url, flac, header)
    data = urllib2.urlopen(req)
    res = data.read()    
    
    res = eval(res)

    speack(res["hypotheses"][0]["utterance"])
    #speack(data.read()["hypotheses"][0]["utterance"])
    time.sleep(12)
