with Kit.Schema.Databases;

package Kit.Parser is

   function Read_Kit_File
     (Path : String)
      return Kit.Schema.Databases.Database_Type;

end Kit.Parser;
