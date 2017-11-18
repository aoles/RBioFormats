import java.io.IOException;

import loci.common.DebugTools;
import loci.common.DataTools;
import loci.common.services.DependencyException;
import loci.common.services.ServiceException;
import loci.common.services.ServiceFactory;
import loci.formats.services.OMEXMLService;
import loci.formats.FormatException;
import loci.formats.IFormatReader;
import loci.formats.IFormatWriter;
import loci.formats.ImageReader;
import loci.formats.ImageWriter;
import loci.formats.ChannelFiller;
import loci.formats.ChannelSeparator;
import loci.formats.DimensionSwapper;
import loci.formats.FormatTools;
import loci.formats.MetadataTools;
import loci.formats.services.OMEXMLService;
import loci.formats.services.OMEXMLServiceImpl;
import loci.formats.ome.OMEXMLMetadata;
import loci.formats.MissingLibraryException;
import loci.formats.meta.DummyMetadata;
import loci.formats.meta.MetadataStore;
import loci.formats.meta.IMetadata;


public final class RBioFormats {
  private static DimensionSwapper reader;
  private static IFormatWriter writer;
  private static MetadataStore omexml;
  private static String dimensionOrder = "XYCZT";
  
  static {
    // reduce verbosity
    DebugTools.enableLogging("ERROR");
  }
  
  public static IFormatReader getReader() {
    if (reader==null) {
      // ChannelFiller: convert indexed color images to RGB images.  
      // ChannelSeparator: split RGB images into 3 separate grayscale images
      // DimensionSwapper: enable setting output dimension order
      reader = new DimensionSwapper(new ChannelSeparator(new ChannelFiller()));
    }
    return reader;
  }
  
  public static IFormatWriter getWriter() {
    if (writer==null) {
      writer = new ImageWriter();
    }
    return writer;
  }
  
  public static MetadataStore getOMEXML() {
    return omexml;
  }
  
  public static String getCurrentFile() {
    return reader.getCurrentFile();
  }
  
  // setup the reader
  public static void setupReader(String file, boolean filter, boolean proprietary, boolean xml) throws FormatException, IOException {
  
    // set metadata options
    reader.setMetadataFiltered(filter);
    reader.setOriginalMetadataPopulated(proprietary);
    reader.setFlattenedResolutions(false);
    
    // omexml
    if (xml) {
      omexml = (MetadataStore) getMetadataStore();
    }
    else {
      omexml = new DummyMetadata();
    }
    reader.setMetadataStore(omexml);
    
    // initialize file   
    reader.setId(file);
    reader.setOutputOrder(dimensionOrder);
  }
  
  // setup the writer
  public static void setupWriter(String file, int[] dim, int series, String pixelType) throws ServiceException, DependencyException, FormatException, IOException {
    int sizeX = dim[0];
    int sizeY = dim[1];
    int sizeZ = dim[3];
    int sizeC = dim[2];
    int sizeT = dim[4];
    
    ServiceFactory factory = new ServiceFactory();
    OMEXMLService service = factory.getInstance(OMEXMLService.class);
    IMetadata meta = service.createOMEXMLMetadata();

    MetadataTools.populateMetadata(meta, series, null, false, dimensionOrder, pixelType, sizeX, sizeY, sizeZ, sizeC, sizeT, sizeC);
    
    writer.setMetadataRetrieve(meta);
    writer.setId(file);
  }
  
  public static Object readPixels(int i, int x, int y, int w, int h, boolean normalize) throws FormatException, IOException {
    int pixelType = reader.getPixelType();
    int size = w * h * FormatTools.getBytesPerPixel(pixelType) * reader.getRGBChannelCount();
    
    byte[] buf = new byte[size];
    
    reader.openBytes(i, buf, x, y, w, h);
    
    int bpp = FormatTools.getBytesPerPixel(pixelType);
    boolean fp = FormatTools.isFloatingPoint(pixelType);
    boolean little = reader.isLittleEndian();
    
    if (normalize)
      return normalizedDataArray(buf, bpp, fp, little, pixelType);
    else
      return rawDataArray(buf, bpp, FormatTools.isSigned(pixelType), fp, little);
  }
  
  private static IMetadata getMetadataStore() throws FormatException {
    try {
      ServiceFactory factory = new ServiceFactory();
      OMEXMLService service = factory.getInstance(OMEXMLService.class);
      return service.createOMEXMLMetadata();
    }
    catch (DependencyException de) {
      throw new MissingLibraryException(OMEXMLServiceImpl.NO_OME_XML_MSG, de);
    }
    catch (ServiceException se) {
      throw new FormatException(se);
    }
  }
  
  private static Object rawDataArray(byte[] b, int bpp, boolean signed, boolean fp, boolean little) {
    // unsigned types need to be stored in a longer signed type
    int type = signed ? bpp : bpp * 2;
    int len = b.length / bpp;
    
    // int8
    // convert bytes to shorts in order to have integer rather than raw vector in R
    if (type == 1) {
      short[] s = new short[len];
      for (int i=0; i<len; i++) {
        s[i] = (short) b[i];
      }
      return s;
    }
    // uint8, int16
    else if (type == 2) {
      short[] s = new short[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        s[i] = DataTools.bytesToShort(b, j, bpp, little);
      }
      return s;
    }
    // float
    else if (type == 4 && fp) {
      float[] f = new float[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        f[i] = DataTools.bytesToFloat(b, j, bpp, little);
      }
      return f;
    }
    // uint16
    else if (type == 4 && !signed) {
      int[] l = new int[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        l[i] = DataTools.bytesToInt(b, j, bpp, little);
      }
      return l;
    }
    // int32
    // we cannot use 32bit ints for signed values because
    // the minimal int value in Java -2^31 = -2147483648 represents NA in R
    // https://github.com/s-u/rJava/issues/39#issuecomment-72207912
    else if (type == 4) {
      double[] d = new double[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        d[i] = (double) DataTools.bytesToInt(b, j, bpp, little);
      }
      return d;
    }
    // double
    else if (type == 8 && fp) {
      double[] d = new double[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        d[i] = DataTools.bytesToDouble(b, j, bpp, little);
      }
      return d;
    }
    // uint32
    // use Java long which is returned as double in R
    else if (type == 8) {
      long[] l = new long[len];
      for (int i=0, j=0; i<len; i++, j+=bpp) {
        l[i] = DataTools.bytesToLong(b, j, bpp, little);
      }
      return l;
    }
    return null;
  }

  private static double[] normalizedDataArray(byte[] b, int bpp, boolean fp, boolean little, int pixelType) {
    double[] data = new double[b.length / bpp];
    
    // floating point normalization 
    if (fp) {
      double min = Double.MAX_VALUE, max = Double.MIN_VALUE;
      
      if (bpp == 4) {
        for (int i=0; i<data.length; i++) {
          data[i] = (double) DataTools.bytesToFloat(b, i * bpp, bpp, little);
          if (data[i] == Double.POSITIVE_INFINITY || data[i] == Double.NEGATIVE_INFINITY) 
            continue;
          else {
            if (data[i] < min) min = data[i];
            if (data[i] > max) max = data[i];
          }
        }
      }
      else if (bpp == 8) {
        for (int i=0; i<data.length; i++) {
          data[i] = DataTools.bytesToDouble(b, i * bpp, bpp, little);
          if (data[i] == Double.POSITIVE_INFINITY || data[i] == Double.NEGATIVE_INFINITY) 
            continue;
          else {
            if (data[i] < min) min = data[i];
            if (data[i] > max) max = data[i];
          }
        }      
      }
      else return null;
      
      // normalize min => 0.0, max => 1.0
      double range = max - min;
      for (int i=0; i<data.length; i++) {
        if (data[i] == Double.POSITIVE_INFINITY) data[i] = 1.0;
        else if (data[i] == Double.NEGATIVE_INFINITY) data[i] = 0.0;
        else data[i] = (data[i] - min) / range;
      }
    }
    else {
      long[] minmax = FormatTools.defaultMinMax(pixelType);
      double min = minmax[0], max = minmax[1];
       
      // true bpp value overrides the default
      int bitsPerPixel = reader.getCoreMetadataList().get(reader.getCoreIndex()).bitsPerPixel;
      if ( !FormatTools.isSigned(pixelType) &&  bitsPerPixel < FormatTools.getBytesPerPixel(pixelType) * 8) max = Math.pow(2, bitsPerPixel) - 1;	
      
      double range = max - min;
      for(int i = 0; i < data.length; ++i) data[i] = ((double) DataTools.bytesToLong(b, i * bpp, bpp, little) - min) / range;      
    }
    
    return data;
  }
  
  public static void writePixels(String file, int[] data, int[] dim, int series, String mode) throws Exception {
    boolean little = false; //TODO: need to revise this
    byte[] b;
    
    switch (FormatTools.pixelTypeFromString(mode)) {
      case FormatTools.INT8:
      case FormatTools.UINT8:
        b = new byte[data.length ];
        for (int j=0; j<data.length; j++)
          b[j] = (byte) data[j];
        break;
      case FormatTools.INT16:
      case FormatTools.UINT16:
        short[] s = new short[data.length];
        for (int j=0; j<data.length; j++)
          s[j] = (short) data[j];
        b = DataTools.shortsToBytes(s, little);
        break;
      case FormatTools.INT32:
      case FormatTools.UINT32:
        b = DataTools.intsToBytes(data, little);
        break;
      case FormatTools.FLOAT:
        float[] f = new float[data.length];
        for (int j=0; j<data.length; j++)
          f[j] = (float) data[j];
        b = DataTools.floatsToBytes(f, little);
        break;
      case FormatTools.DOUBLE:
        double[] d = new double[data.length];
        for (int j=0; j<data.length; j++)
          d[j] = (double) data[j];
        b = DataTools.doublesToBytes(d, little);
        break;
      default:
        b= null;
        break;
    }
    
    writeBytes(file, b, dim, series, mode);
  }
  
  public static void writePixels(String file, double[] data, int[] dim, int series, String mode) throws Exception {
    boolean little = false; //TODO: need to revise this
    byte[] b;
    
    switch (FormatTools.pixelTypeFromString(mode)) {
      case FormatTools.INT8:
      case FormatTools.UINT8:
        b = new byte[data.length];
        for (int j=0; j<data.length; j++)
          b[j] = (byte) data[j];
        break;
      case FormatTools.INT16:
      case FormatTools.UINT16:
        short[] s = new short[data.length];
        for (int j=0; j<data.length; j++)
          s[j] = (short) data[j];
        b = DataTools.shortsToBytes(s, little);
        break;
      case FormatTools.INT32:
      case FormatTools.UINT32:
        int[] i = new int[data.length];
        for (int j=0; j<data.length; j++)
          i[j] = (int) (long) data[j];
        b = DataTools.intsToBytes(i, little);
        break;
      case FormatTools.FLOAT:
        float[] f = new float[data.length];
        for (int j=0; j<data.length; j++)
          f[j] = (float) data[j];
        b = DataTools.floatsToBytes(f, little);
        break;
      case FormatTools.DOUBLE:
        b = DataTools.doublesToBytes(data, little);
        break;
      default:
        b = null;
        break;
    }
    
    writeBytes(file, b, dim, series, mode);
  }
  
  private static void writeBytes(String file, byte[] img, int[] dim, int series, String pixelType) throws IOException, FormatException {

    
    writer.saveBytes(series, img);
  }
  
}
