# Makefile for erlawys.

.SUFFIXES:	.erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = aws_util aws_ec2_xml aws_ec2 aws_ec2_test aws_fps_xml aws_fps

all:	compile

compile:	${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
	