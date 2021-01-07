/*
Program for a class to transform images from equilateral projections to mollweide and Lambert, is off by a pixel for certain odd dimensions of images
*/

package main

import (
	"fmt"
	"image"
	"image/png"
	"math"
	"os"
	"strconv"
)

/**
If error is encountered, display error to user and exit program with an error code
*/
func errFound(err error) {
	fmt.Println("ERROR: ", err)
	os.Exit(-1)
}

/**
Function to build Lambert transformation
*/
func buildLambert(img image.Image, standLatDeg float64) image.Image {
	//Convert from degrees to radians
	stanLat := math.Pi * standLatDeg / 180

	intHeight := img.Bounds().Max.Y
	height := float64(intHeight)

	//Determine width of output based on height and latitude
	outWidth := height*math.Pi*math.Pow(math.Cos(stanLat), 2) + 0.5
	intOutWidth := int(outWidth)

	//determine stretch factor from widths
	widthRatio := float64(img.Bounds().Max.X) / outWidth

	returnedImage := image.NewNRGBA(image.Rect(0, 0, intOutWidth, intHeight))

	//For each row of output image
	//I tried to parallelize this using goroutines, but the overhead was not worth it
	//It massively increased execution time
	for y := 0; y < intHeight; y++ {

		//Use center of pixel, rather than true coordinate
		latitude := math.Asin(((2 / height) * (float64(y) + 0.5)) - 1)

		//latitude ranges from 0-pi, figure out ratio and grab it from the height
		inpY := (latitude/math.Pi + 0.5) * height

		//For each column in that row
		for x := 0; x < intOutWidth; x++ {

			//calculate input's x from width ratio, and set output pixel
			returnedImage.Set(x, y, img.At(int((float64(x)+0.5)*widthRatio), int(inpY)))
		}

	}
	return returnedImage
}

func buildMollewide(img image.Image) image.Image {
	//Avoid making multiple conversions later
	intWidth := img.Bounds().Max.X
	intHeight := img.Bounds().Max.Y

	height := float64(intHeight)
	width := float64(intWidth)

	meridian := width / 2
	equator := height / 2

	ellipseHeight := meridian * meridian
	ellipseWidth := equator * equator

	//output image has same dimensions as input
	returnedImage := image.NewNRGBA(image.Rect(0, 0, intWidth, intHeight))

	for y := 0; y < intHeight; y++ {
		//get center of pixel with relation to center line of image,
		equOffset := float64(y) + 0.5 - equator

		// find the y coordinate on the ellipse
		ellipseY := equOffset * equOffset / ellipseWidth

		theta := math.Asin(equOffset / equator)

		lat := math.Asin((2*theta + math.Sin(2*theta)) / math.Pi)

		// latitude ranges from 0-pi, use the ratio to find source pixel's relationship to height
		sourceY := (lat/math.Pi + 0.5) * height

		xScale := width / (width * math.Cos(theta))

		// go through the x pixels of output image
		for x := 0; x < intWidth; x++ {

			//find distance from center line
			meridianOffset := float64(x) + 0.5 - meridian

			//find X coordinate on ellipse
			ellipseX := meridianOffset * meridianOffset / ellipseHeight

			// if pixel is outside the radius of the ellipse
			if ellipseX+ellipseY >= 1 {
				returnedImage.Set(x, y, image.White)

			} else {

				sourceX := meridian + meridianOffset * xScale
				returnedImage.Set(x, y, img.At(int(sourceX), int(sourceY)))
			}
		}
	}
	return returnedImage
}

func main() {
	usage := "USAGE: ./main infile outfile <Cylindrical?> <Standard Latitude>"
	argc := len(os.Args)
	if argc < 3 {
		fmt.Println(usage)
		os.Exit(-1)
	}

	fileIn, err := os.Open(os.Args[1])
	if err != nil {
		errFound(err)
	}

	img, err := png.Decode(fileIn)
	if err != nil {
		errFound(err)
	}

	var outImage image.Image

	// transform based on arguments
	switch argc {
	case 3:
		outImage = buildMollewide(img)
		break
	case 4:
		outImage = buildLambert(img, 0)
		break
	case 5:
		stdlat, err := strconv.ParseFloat(os.Args[4], 64)
		if err != nil {
			errFound(err)
		}
		outImage = buildLambert(img, stdlat)
		break
	default:
		fmt.Println(usage)
		os.Exit(-1)
	}

	outFile, err := os.Create(os.Args[2])
	if err != nil {
		errFound(err)
	}

	err = png.Encode(outFile, outImage)
	if err != nil {
		errFound(err)
	}
	return
}
