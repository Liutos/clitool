import datetime
import os
import random
import shutil


def main():
    now = datetime.datetime.now()
    current_month = now.month
    current_year = now.year
    if current_month == 12:
        dir_name = '%d-01' % (current_year + 1)
    else:
        dir_name = '%d-%02d' % (current_year, current_month + 1)

    dir_path = '/Volumes/Elements/图片/头像/%s' % dir_name
    if not os.path.exists(dir_path):
        os.mkdir(dir_path)

    images = [filename for filename in os.listdir('/Volumes/Elements/图片/头像/') if filename.endswith('.jpg')]
    selected_images = random.sample(images, 9)
    NAMES = [
        '百度',
        '豆瓣',
        '微博',
        '知乎',
        'GitHub',
        'QQ',
        'SegmentFault',
        'Twitter',
        'v2ex',
    ]
    for i, image in enumerate(selected_images):
        source_path = '/Volumes/Elements/图片/头像/%s' % image
        dest_path = '%s/%s.jpg' % (dir_path, NAMES[i])
        shutil.move(source_path, dest_path)


if __name__ == '__main__':
    main()
