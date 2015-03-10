import java.io.IOException;

import loci.common.DebugTools;
import loci.common.DataTools;
import loci.common.services.DependencyException;
import loci.common.services.ServiceException;
import loci.common.services.ServiceFactory;
import loci.formats.FormatException;
import loci.formats.IFormatReader;
import loci.formats.ImageReader;
import loci.formats.ChannelFiller;
import loci.formats.ChannelSeparator;
import loci.formats.DimensionSwapper;
import loci.formats.FormatTools;
import loci.formats.services.OMEXMLService;
import loci.formats.services.OMEXMLServiceImpl;
import loci.formats.ome.OMEXMLMetadata;
import loci.formats.MissingLibraryException;
import loci.formats.meta.DummyMetadata;
import loci.formats.meta.MetadataStore;

public final class RBioFormats {   
  public static DimensionSwapper reader;
  public static MetadataStore omexml;
  
  // initialize reader
  static {
     // reduce verbosity
    DebugTools.enableLogging("ERROR");
  
    // ChannelFiller: convert indexed color images to RGB images.  
    // ChannelSeparator: split RGB images into 3 separate grayscale images
    // DimensionSwapper: enable setting output dimension order
    reader = new DimensionSwapper(new ChannelSeparator(new ChannelFiller()));
  }
  
  // setup the reader
  public static void setupReader(String file, boolean filter, boolean proprietary, boolean xml) throws FormatException, IOException {
    // set metadata options
    reader.setMetadataFiltered(filter);
    reader.setOriginalMetadataPopulated(proprietary);
    
    // omexml
    if (xml) try {  
      ServiceFactory factory = new ServiceFactory();
      OMEXMLService service = factory.getInstance(OMEXMLService.class);
      omexml = service.createOMEXMLMetadata();
    }
    catch (DependencyException de) {
      throw new MissingLibraryException(OMEXMLServiceImpl.NO_OME_XML_MSG, de);
    }
    catch (ServiceException se) {
      throw new FormatException(se);
    }
    else {
      omexml = new DummyMetadata();
    }
    reader.setMetadataStore(omexml);
    
    // initialize file   
    reader.setId(file);    
    reader.setOutputOrder("XYCZT");
  }
 
  private static double[] bytesToDoubles(byte[] b, int bpp, boolean fp, boolean little) {
    double[] d = new double[b.length / bpp];
    
    /*
    if (bpp == 1)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToShort(b, i, 1, little);
    else if (bpp == 2)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToShort(b, i * 2, 2, little);
    else if (bpp == 4 && fp)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToFloat(b, i * 4, 4, little);
    else if (bpp == 4)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToInt(b, i * 4, 4, little);
    else if (bpp == 8 && fp)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToDouble(b, i * 8, 8, little);
    else if (bpp == 8)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToLong(b, i * 8, 8, little);
    */
    
    if (fp)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToDouble(b, i * bpp, bpp, little);
    else
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToLong(b, i * bpp, bpp, little);
    
    return d;
  }
  
  
  private static double[] bytesToDoubles2(byte[] b, int bpp, boolean fp, boolean little, int normalize) {
    double[] d = new double[b.length / bpp];
    
    /*
    if (bpp == 1)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToShort(b, i, 1, little);
    else if (bpp == 2)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToShort(b, i * 2, 2, little);
    else if (bpp == 4 && fp)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToFloat(b, i * 4, 4, little);
    else if (bpp == 4)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToInt(b, i * 4, 4, little);
    else if (bpp == 8 && fp)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToDouble(b, i * 8, 8, little);
    else if (bpp == 8)
      for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToLong(b, i * 8, 8, little);
    */
    
    if (fp) {
      for (int i=0; i<d.length; i++) d[i] = DataTools.bytesToDouble(b, i * bpp, bpp, little);
    }
    else {
      if (normalize > 0) {
	double max = Math.pow(2, normalize) - 1;
	for (int i=0; i<d.length; i++) d[i] = DataTools.bytesToLong(b, i * bpp, bpp, little) / max;
      }
      else {
	for (int i=0; i<d.length; i++) d[i] = (double) DataTools.bytesToLong(b, i * bpp, bpp, little);
      }
    }
    
    return d;
  }
  
  public static double[] readPixels(int[] planes, boolean normalize) throws FormatException, IOException {
    
    int size = reader.getSizeX() * reader.getSizeY();
    int offset = 0;
        
    int pixelType = reader.getPixelType();
    double[] res = new double[size * planes.length];
    
    for(int i = 0; i < planes.length; ++i, offset += size)
      System.arraycopy(bytesToDoubles(reader.openBytes(planes[i]-1),
				      FormatTools.getBytesPerPixel(pixelType),
				      FormatTools.isFloatingPoint(pixelType),
				      reader.isLittleEndian()), 0, res, offset, size);
    
    
    
    if (normalize) {
      double max = Math.pow(2, reader.getBitsPerPixel()) - 1;
      for(int i = 0; i < res.length; ++i) res[i] /= max;
    }
    
    return res;
  }
  
  public static double[] readPixels2(int i, boolean normalize) throws FormatException, IOException {
    int pixelType = reader.getPixelType();
    
    return bytesToDoubles2(
      reader.openBytes(i),
      FormatTools.getBytesPerPixel(pixelType),
      FormatTools.isFloatingPoint(pixelType),
      reader.isLittleEndian(),
      normalize ? reader.getBitsPerPixel() : 0
    );
  }

}
