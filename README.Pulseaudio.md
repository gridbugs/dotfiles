# Pulseaudio Notes

## Mixing a mic with speaker output

Find out the name of the speaker output by running `pactl list sinks short`:
```
$ pactl list sinks short
1       alsa_output.pci-0000_0d_00.4.analog-stereo      module-alsa-card.c      s16le 2ch 44100Hz       RUNNING
51      alsa_output.pci-0000_0b_00.1.hdmi-stereo        module-alsa-card.c      s16le 2ch 44100Hz       IDLE
```

Create a null output whose monitor will be a source which combines the microphone and speaker output:
```
$ pactl load-module module-null-sink sink_name=both sink_properties=device.description=Both-mic-and-speakers
```

The name of the sink is "both". Its description is "Both-mic-and-speakers" - this is the device name you'll see in `pavucontrol`.

Send the speaker output to the combined sink, e.g.:
```
$ pactl load-module module-loopback source=alsa_output.pci-0000_0d_00.4.analog-stereo.monitor sink=both
```

Send the mic output to the combined sink:
```
$ pactl load-module module-loopback sink=both
```

Note that this will just send the default mic to the combined sink. You can pass a different source with `source=...` or change it later in `pavucontrol`.

Now use `pavucontrol` to change the input device in the "Recording" tab to "Monitor of Both-mic-and-speakers".
