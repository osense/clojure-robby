#!/usr/bin/python2

import sys
import os
import ast
from PIL import Image
from PIL import ImageFont
from PIL import ImageDraw

frame_size = (400, 440)
tile_size = 20
black = (0, 0, 0)
grey = (127, 127, 127)
yellow = (255, 215, 0)
font = ImageFont.truetype("/usr/share/fonts/TTF/OxygenMono-Regular.ttf", 15)

infile = sys.argv[1]
frames = []
with open(infile, 'r') as f:
    for line in f:
        frames.append(ast.literal_eval(line))


def path(d, x, y):
    d.rectangle([x*tile_size, y*tile_size, (x+1)*tile_size, (y+1)*tile_size], outline = black)

def wall(d, x, y):
    d.rectangle([x*tile_size, y*tile_size, (x+1)*tile_size, (y+1)*tile_size], fill = grey)

def gold(d, x, y):
    path(d, x, y)
    #size offset
    o = tile_size/5
    d.ellipse([x*tile_size + o, y*tile_size + o, (x+1)*tile_size - o, (y+1)*tile_size - o], fill = yellow)

def robot(d, x, y):
    path(d, x, y)
    d.rectangle([x*tile_size, y*tile_size, (x+1)*tile_size, (y+1)*tile_size], fill = black)

def draw_frame(index, (score, the_map)):
    img = Image.new('RGB', frame_size, "white")
    draw = ImageDraw.Draw(img)
    # draw footer: score and frame index
    foot_y = frame_size[1] - 2*tile_size + tile_size/2
    draw.text((10, foot_y), 'frame ' + str(index), black, font=font)
    draw.text((frame_size[1] - frame_size[0]/2, foot_y), 'score: ' + str(score), black, font=font)
    #draw tiles
    y = -1
    for row in the_map:
        y = y + 1
        x = -1
        for tile in row:
            x = x + 1
            if tile == 'p':
                path(draw, x, y)
            elif tile == 'w':
                wall(draw, x, y)
            elif tile == 'g':
                gold(draw, x, y)
            elif tile == 'R':
                robot(draw, x, y)
    img.save('/tmp/frame' + str(index) + '.gif', 'GIF')

from subprocess import call
def draw_all():
    i = 0
    for frame in frames:
        draw_frame(i, frame)
        i = i+1
    (outname, _) = os.path.splitext(infile)
    f = open(outname + '.gif', "w")
    call("gifsicle --delay=50 --loop /tmp/frame{0.." + str(i-1) + "}.gif", stdout=f, shell=True)

draw_all()

