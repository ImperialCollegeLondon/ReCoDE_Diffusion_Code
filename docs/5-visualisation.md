# 5 - Data Visualisation

## GnuPlot

## Python

## ParaView

One particular output viewing software that may be of interest to some readers is ParaView. ParaView is a widely used software for data analysis and visualisation within the scientific community. While ParaView could read in our text file outputs to generate a graph, this does not demonstrate much of the power of the software. Instead, the code has been written such that it will generate a 2-Dimensional plot of the flux that can be manipulated in ParaView.

To do so, we must generate an output file which uses the VTK file format and translate our resulting data to this format. We have also made use of the more modern .VTU file type, a form of VTK file which makes use of the html file structure. In addition to the readability we gain from such a structure, it is also recommended that users avoid the .VTK legacy file type if possible. A snippet from **OutputFile.vtu** can be seen below, where we have specified that this is a 'VTKFile' using the 'Unstructured Grid' format. Contained within the Unstructured Grid, we define the number of points and cells within the problem. We then go on to describe the relevant point and cell data. The VTK file format is described here: https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf.

```xml
<?xml version="1.0"?>
<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">
  <UnstructuredGrid>
    <Piece NumberOfPoints="18" NumberOfCells="8"></Piece></UnstructuredGrid>
</VTKFile>
```
