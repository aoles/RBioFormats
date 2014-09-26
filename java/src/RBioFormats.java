import loci.common.DataTools;
import loci.common.DebugTools;
import loci.common.services.ServiceFactory;
import loci.formats.ChannelFiller;
import loci.formats.ChannelSeparator;
import loci.formats.DimensionSwapper;
import loci.formats.FormatTools;
import loci.formats.IFormatReader;
import loci.formats.ImageReader;
import loci.formats.meta.MetadataStore;
import loci.formats.services.OMEXMLService;
import loci.formats.ome.OMEXMLMetadata;

public final class RBioFormats {
    
  public static String sayHello() {
    String result = new String("Hello Java World!");
    return result;
  }
  
}