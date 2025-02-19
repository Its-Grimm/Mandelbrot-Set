import matplotlib.pyplot as plt
from PIL import Image
import numpy as np


def get_points_from_file():
    with open("points.txt", "r") as file:
        points = [
            complex(line.strip().replace("\"", "").replace("i", "j")) 
            for line in file
        ]
    return points

def draw_points(points):
    x_arr, y_arr = [], []
    xy_size = 4000
    for num in points:
        x_arr.append(num.real)
        y_arr.append(num.imag)

    x_points = np.array(x_arr)
    y_points = np.array(y_arr)

    im = Image.new(mode="RGB", size=(xy_size, xy_size), color=(255, 255, 255))
    for x_point, y_point in zip(x_points, y_points):
        im.putpixel((int(x_point * ((xy_size // 2) // 2)) + (xy_size // 2), int(y_point * ((xy_size // 2) // 2)) + (xy_size // 2)), (0, 0, 0, 255))

    im.save('Mandlebrot-Set-Img.png')

def graph_points(points):
    graph_min_size, graph_max_size = -2.0000, 2.0000

    x_arr, y_arr = [], []
    for num in points:
        x_arr.append(num.real)
        y_arr.append(num.imag)

    x_points = np.array(x_arr)
    y_points = np.array(y_arr)
    
    plt.plot(x_points, y_points, ',', color='k', ms=2)

    plt.xlim(graph_min_size, graph_max_size)
    plt.ylim(graph_min_size, graph_max_size)
    plt.xlabel('Real')
    plt.ylabel('Imaginary')
    
    plt.savefig('Mandelbrot-Set-Graph.png', dpi=80)


def main():
    points = get_points_from_file()

    # graph_points(points)
    draw_points(points)


if __name__ == "__main__":
    main()
