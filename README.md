# neImageTools
neImageTools is a set of components which extends the TImageList and TImage functionality by enabling them to load image files from a directory in design and run time

# neImageList
neImageList loads image files from a directory

The component exposes the following properties:
1. BaseScale: The component needs to calculate the size of the images in order to load them to a MultiResBitmap property. This is the scale that is considered of factor 1x
2. DelimiterString: The filenames of images, typically, include the scale factor (e.g. add_button@1.5x.png). This property indicates the separator string, which is '@' by default
3. Directory: The directory with the image files. Relative directories are accepted. Multiple directories can be supplied separated by ';'
4. FilterTag: Sometimes it is more convenient to load a specific set of images, eg. all the add_button* images. This property filters the filenames to be loaded

# neImage
neImage extends the classic TImage and adds properties to load images from a directory. The properties are the same as in neImageList with the difference of the FilterTag. In neImage the property is called ImageTag and it is compulsory

Many times you want to supply images programmatically. TneImage has a dedicated event (OnGetBitmap) which exposes the Bitmap of TneImage and fires just before Paint is called.

# How To Use

Build and install the package in SourceCode/Packages/Dxxx according to the Delphi installation you want to use. Then, just drag and drop the components to a multi-device form


#Documentation
There is documentation about the classes, the methods and properties. Please check the "Documentation" folder.


# To-do/Known Issues

1. The image files are being loaded in a normal procedure. A threaded approach would improve responsiveness
2. The processing of the directories with the images is not thread-safe
3. When an image or a list of images are loaded in the components and the application is executed in Windows, the images show correctly. On Android, the components do not store the images in memory unless the image paths are supplied programmatically. It is unclear whether the images in Android are indeed not loaded to memory or the directories in design-time are lost under Android 
4. There are some cases where the component in the IDE is not responsive although the directory and the other properties are correct. If this is the case provide a wrong directory, hit Enter and then re-enter the correct one. This should enforce the component to reload the files. Alternatively, drop a new components

*** Please feel free to provide suggestions or pull requests ***

#Bugs, Suggestions, Comments and General Contact
I hope you find the component useful. If you have any comments or ideas, have spotted any bugs or have any suggestions about changes to the code use the "Issues" tab on github or drop me an email at j_kour@hotmail.com

Thanks!

John

