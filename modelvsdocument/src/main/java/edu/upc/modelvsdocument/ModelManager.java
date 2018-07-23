package edu.upc.modelvsdocument;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.model.BpmnModel;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.*;

public class ModelManager {

    public static BpmnModel readModel (String path) throws Exception {
        FileReader modelFile = new FileReader(path);
        XMLInputFactory factoryXML = XMLInputFactory.newInstance();
        XMLStreamReader xtr = factoryXML.createXMLStreamReader(modelFile);
        return new BpmnXMLConverter().convertToBpmnModel(xtr);
    }

    public static BpmnModel readModelFromString (String model_string) throws Exception {
        Reader reader = new StringReader(model_string);
        XMLInputFactory factoryXML = XMLInputFactory.newInstance();
        XMLStreamReader xtr = factoryXML.createXMLStreamReader(reader);
        return new BpmnXMLConverter().convertToBpmnModel(xtr);
    }

}
