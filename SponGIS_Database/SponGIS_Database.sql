view_distinct_taxon_names
 SELECT DISTINCT taxon."scientificNameID",
    taxon."scientificName",
    taxon."scientificNameAuthorship"
   FROM taxon;

view_species_occurrences
 SELECT extendedoccurrence."scientificNameID",
    event."decimalLatitude",
    event."decimalLongitude",
    concat('[', event."decimalLongitude", ', ', event."decimalLatitude", ']') AS concat
   FROM extendedoccurrence
     JOIN event ON extendedoccurrence."eventID" = event."eventID";

view_species_occurrences_geojson
 SELECT json_build_object('type', 'FeatureCollection', 'features', json_agg(json_build_object('type', 'Feature', 'id', extendedoccurrence."scientificNameID", 'geometry', st_asgeojson(st_setsrid(st_makepoint(event."decimalLongitude"::double precision, event."decimalLatitude"::double precision), 4326))::json, 'properties', json_build_object('feat_type', 'Point', 'scientificNameID', extendedoccurrence."scientificNameID")))) AS json_build_object
   FROM event
     JOIN extendedoccurrence ON event."eventID" = extendedoccurrence."eventID";


function - get_occurrences_geojson - Single Species Version
SELECT json_build_object(
'type', 'FeatureCollection',

'features', json_agg(
    json_build_object(
        'type',       'Feature',
        'id',         extendedoccurrence."scientificNameID",
        'geometry',   ST_AsGeoJSON(ST_SetSRID(ST_MakePoint(event."decimalLongitude"::double precision, event."decimalLatitude"::double precision), 4326))::json,
        'properties',
        json_build_object(
    					'feat_type', 'Point',
    					'scientificNameID', extendedoccurrence."scientificNameID"
                         )

    )

)
) AS null
FROM event
JOIN extendedoccurrence ON event."eventID" = extendedoccurrence."eventID"
WHERE extendedoccurrence."scientificNameID" = j::json->>'id';

function - get_occurrences_geojson - Multiple & Single Species Version
SELECT json_build_object(
'type', 'FeatureCollection',

'features', json_agg(
    json_build_object(
        'type',       'Feature',
        'id',         extendedoccurrence."scientificNameID",
        'geometry',   ST_AsGeoJSON(ST_SetSRID(ST_MakePoint(event."decimalLongitude"::double precision, event."decimalLatitude"::double precision), 4326))::json,
        'properties',
        json_build_object(
    					'feat_type', 'Point',
    					'scientificNameID', extendedoccurrence."scientificNameID",
    					'scientificName', extendedoccurrence."scientificName",
    					'eventID', extendedoccurrence."eventID",
    					'basisOfRecord', extendedoccurrence."basisOfRecord",
    					'minimumDepthInMeters', event."minimumDepthInMeters",
    					'maximumDepthInMeters', event."maximumDepthInMeters",
    					'eventDate', event."eventDate"
                         )

    )

)
) AS null
FROM event
JOIN extendedoccurrence ON event."eventID" = extendedoccurrence."eventID"
WHERE extendedoccurrence."scientificNameID" IN (select x->>'scientificNameID' FROM json_array_elements(j::json) x);






CREATE TABLE "public"."event" (
    "dataID" text,
    "eventID" text,
    "parentEventID" text,
    "eventDate" text,
    "eventRemarks" text,
    "habitat" text,
    "locationID" text,
    "continent" text,
    "waterBody" text,
    "country" text,
    "locality" text,
    "minimumDepthInMeters" numeric,
    "maximumDepthInMeters" numeric,
    "decimalLatitude" text,
    "decimalLongitude" text,
    "coordinateUncertaintyInMeters" text,
    "geodeticDatum" text,
    "locationAccordingTo" text,
    "locationRemarks" text,
    "footprintWKT" text
);
COMMENT ON COLUMN "public"."event"."dataID" IS 'Universally Unique identifier for the dataset in SponGIS. Links to Dataset Table.';
COMMENT ON COLUMN "public"."event"."eventID" IS 'An identifier for the set of information associated with an Event (something that occurs at a place and time).';
COMMENT ON COLUMN "public"."event"."parentEventID" IS 'An identifier for the broader Event that groups this and potentially other Events.';
COMMENT ON COLUMN "public"."event"."eventDate" IS 'The date-time or interval during which an Event occurred. For occurrences, this is the date-time when the event was recorded.';
COMMENT ON COLUMN "public"."event"."eventRemarks" IS 'Comments or notes about the Event.';
COMMENT ON COLUMN "public"."event"."habitat" IS 'A category or description of the habitat in which the Event occurred';
COMMENT ON COLUMN "public"."event"."locationID" IS 'An identifier for the set of location information.';
COMMENT ON COLUMN "public"."event"."continent" IS 'The name of the continent in which the Location occurs.';
COMMENT ON COLUMN "public"."event"."waterBody" IS 'The name of the water body in which the Location occurs.';
COMMENT ON COLUMN "public"."event"."country" IS 'The name of the country or major administrative unit in which the Location occurs.';
COMMENT ON COLUMN "public"."event"."locality" IS 'The specific description or name of the place.';
COMMENT ON COLUMN "public"."event"."minimumDepthInMeters" IS 'The lesser depth of a range of depth below the local surface, in meters.';
COMMENT ON COLUMN "public"."event"."maximumDepthInMeters" IS 'The greater depth of a range of depth below the local surface, in meters.';
COMMENT ON COLUMN "public"."event"."decimalLatitude" IS 'The geographic latitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location.';
COMMENT ON COLUMN "public"."event"."decimalLongitude" IS 'The geographic longitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location.';
COMMENT ON COLUMN "public"."event"."coordinateUncertaintyInMeters" IS 'The horizontal distance (in meters) from the given decimalLatitude and decimalLongitude describing the smallest circle containing the whole of the Location';
COMMENT ON COLUMN "public"."event"."geodeticDatum" IS 'Spatial reference system (SRS) upon which the geographic coordinates given in decimalLatitude and decimalLongitude as based.';
COMMENT ON COLUMN "public"."event"."locationAccordingTo" IS 'Information about the source of this Location information. Could be a publication (gazetteer), institution, or team of individuals.';
COMMENT ON COLUMN "public"."event"."locationRemarks" IS 'Comments or notes about the Location.';
COMMENT ON COLUMN "public"."event"."footprintWKT" IS 'A Well-Known Text (WKT) representation of the shape (footprint, geometry) that defines the Location.';

CREATE TABLE "public"."extendedoccurrence" (
    "eventID" text,
    "scientificNameID" text,
    "occurrenceID" text,
    "scientificName" text,
    "scientificNameAuthorship" text,
    "taxonRank" text,
    "identifiedBy" text,
    "dateIdentified" text,
    "identificationReferences" text,
    "identificationRemarks" text,
    "identificationQualifier" text,
    "typeStatus" text,
    "catalogNumber" text,
    "occurrenceRemarks" text,
    "recordedBy" text,
    "occurrenceStatus" text,
    "preparations" text,
    "associatedMedia" text,
    "associatedReferences" text,
    "associatedSequences" text,
    "modified" text,
    "collectionCode" text,
    "basisOfRecord" text,
    "dataGeneralizations" text,
    "dynamicProperties" text
);
COMMENT ON COLUMN "public"."extendedoccurrence"."occurrenceID" IS 'An identifier for the Occurrence (as opposed to a particular digital record of the occurrence). In the absence of a persistent global unique identifier, construct one from a combination of identifiers in the record that will most closely make the occurrenceID globally unique.';
COMMENT ON COLUMN "public"."extendedoccurrence"."scientificName" IS 'The full scientific name, with authorship and date information if known. When forming part of an Identification, this should be the name in lowest level taxonomic rank that can be determined. This term should not contain identification qualifications, which should instead be supplied in the IdentificationQualifier term.';
COMMENT ON COLUMN "public"."extendedoccurrence"."scientificNameAuthorship" IS 'The authorship information for the scientificName formatted according to the conventions of the applicable nomenclaturalCode.';
COMMENT ON COLUMN "public"."extendedoccurrence"."taxonRank" IS 'The taxonomic rank of the most specific name in the scientificName. Recommended best practice is to use a controlled vocabulary.';
COMMENT ON COLUMN "public"."extendedoccurrence"."identifiedBy" IS 'A list (concatenated and separated) of names of people, groups, or organizations who assigned the Taxon to the subject.';
COMMENT ON COLUMN "public"."extendedoccurrence"."identificationReferences" IS 'A list (concatenated and separated) of references (publication, global unique identifier, URI) used in the Identification.';
COMMENT ON COLUMN "public"."extendedoccurrence"."identificationRemarks" IS 'Comments or notes about the Identification.';
COMMENT ON COLUMN "public"."extendedoccurrence"."identificationQualifier" IS 'A brief phrase or a standard term ("cf.", "aff.") to express the determiners doubts about the Identification.';
COMMENT ON COLUMN "public"."extendedoccurrence"."typeStatus" IS 'A list (concatenated and separated) of nomenclatural types (type status, typified scientific name, publication) applied to the subject.';
COMMENT ON COLUMN "public"."extendedoccurrence"."catalogNumber" IS 'An identifier (preferably unique) for the record within the data set or collection.';
COMMENT ON COLUMN "public"."extendedoccurrence"."occurrenceRemarks" IS 'Comments or notes about the Occurrence.';
COMMENT ON COLUMN "public"."extendedoccurrence"."recordedBy" IS 'A list (concatenated and separated) of names of people, groups, or organizations responsible for recording the original Occurrence. The primary collector or observer, especially one who applies a personal identifier (recordNumber), should be listed first.';
COMMENT ON COLUMN "public"."extendedoccurrence"."occurrenceStatus" IS 'A statement about the presence or absence of a Taxon at a Location.';
COMMENT ON COLUMN "public"."extendedoccurrence"."preparations" IS 'A list (concatenated and separated) of preparations and preservation methods for a specimen.';
COMMENT ON COLUMN "public"."extendedoccurrence"."associatedMedia" IS 'A list (concatenated and separated) of identifiers (publication, global unique identifier, URI) of media associated with the Occurrence.';
COMMENT ON COLUMN "public"."extendedoccurrence"."associatedReferences" IS 'A list (concatenated and separated) of identifiers (publication, bibliographic reference, global unique identifier, URI) of literature associated with the Occurrence.';
COMMENT ON COLUMN "public"."extendedoccurrence"."associatedSequences" IS 'A list (concatenated and separated) of identifiers (publication, global unique identifier, URI) of genetic sequence information associated with the Occurrence.';
COMMENT ON COLUMN "public"."extendedoccurrence"."modified" IS 'The most recent date-time on which the resource was changed.';
COMMENT ON COLUMN "public"."extendedoccurrence"."collectionCode" IS 'The name, acronym, coden, or initialism identifying the collection or data set from which the record was derived.';
COMMENT ON COLUMN "public"."extendedoccurrence"."basisOfRecord" IS 'The specific nature of the data record.';
COMMENT ON COLUMN "public"."extendedoccurrence"."dataGeneralizations" IS 'Actions taken to make the shared data less specific or complete than in its original form. Suggests that alternative data of higher quality may be available on request.';
COMMENT ON COLUMN "public"."extendedoccurrence"."dynamicProperties" IS 'A list (concatenated and separated) of additional measurements, facts, characteristics, or assertions about the record. Meant to provide a mechanism for structured content such as key-value pairs.';
COMMENT ON COLUMN "public"."extendedoccurrence"."dateIdentified" IS 'The date on which the subject was identified as representing the Taxon.';

CREATE TABLE "public"."meta" (
    "dataID" text,
    "title" text,
    "created" text,
    "bibliographicCitation" text,
    "description" text,
    "creator" text,
    "footprintWKT" text,
    "rightsHolder" text,
    "accessRights" text,
    "institutionCode" text,
    PRIMARY KEY ("dataID")
);
COMMENT ON COLUMN "public"."meta"."dataID" IS 'Universally Unique identifier for the dataset in SponGIS.';
COMMENT ON COLUMN "public"."meta"."title" IS 'A name given to the resource.';
COMMENT ON COLUMN "public"."meta"."title" IS 'The date this resource was publiched online, not data collection or original publication date.';
COMMENT ON COLUMN "public"."meta"."bibliographicCitation" IS 'Expected Citation as you would like it.';
COMMENT ON COLUMN "public"."meta"."description" IS 'Abstract or concise description.';
COMMENT ON COLUMN "public"."meta"."creator" IS 'An entity primarily responsible for making the resource.';
COMMENT ON COLUMN "public"."meta"."footprintWKT" IS 'A WKT object of a N,S,E,W bounding box that encloses the dataset position.';
COMMENT ON COLUMN "public"."meta"."rightsHolder" IS 'A person or organization owning or managing rights over the resource. Can be institute or individual.';
COMMENT ON COLUMN "public"."meta"."accessRights" IS 'Information about who can access the resource or an indication of its security status. Access Rights may include information regarding access or restrictions based on privacy, security, or other policies.';
COMMENT ON COLUMN "public"."meta"."institutionCode" IS 'The name (or acronym) in use by the institution having custody of the object(s) or information referred to in the record.';

CREATE TABLE "public"."taxon" (
    "taxonID" integer NOT NULL,
    "scientificNameID" bigint,
    "acceptedNameUsageID" text,
    "scientificName" text,
    "scientificNameAuthorship" text,
    "taxonRank" text,
    "taxonRemarks" text,
    "taxonDescription" text,
    "taxonImage" text,
    "higherClassification" text,
    "taxonomicStatus" text,
    "vernacularName" text,
    PRIMARY KEY ("taxonID")
);
COMMENT ON COLUMN "public"."taxon"."scientificNameID" IS 'An identifier for the nomenclatural (not taxonomic) details of a scientific name.';
COMMENT ON COLUMN "public"."taxon"."acceptedNameUsageID" IS 'An identifier for the name usage (documented meaning of the name according to a source) of the currently valid (zoological) or accepted (botanical) taxon.';
COMMENT ON COLUMN "public"."taxon"."scientificName" IS 'The full scientific name (with authorship and date information if known. => add to scientificNameAuthorship).';
COMMENT ON COLUMN "public"."taxon"."scientificNameAuthorship" IS 'The full scientific name (with authorship and date information if known. => add to scientificNameAuthorship).';
COMMENT ON COLUMN "public"."taxon"."taxonRank" IS 'The taxonomic rank of the most specific name in the scientificName.';
COMMENT ON COLUMN "public"."taxon"."taxonRemarks" IS 'Comments or notes about the taxon or name.';
COMMENT ON COLUMN "public"."taxon"."taxonDescription" IS 'Brief description of the taxon if applicable.';
COMMENT ON COLUMN "public"."taxon"."taxonImage" IS 'Image of the taxon.';
COMMENT ON COLUMN "public"."taxon"."higherClassification" IS 'A list (concatenated and separated) of taxa names terminating at the rank immediately superior to the taxon referenced in the taxon record.';
COMMENT ON COLUMN "public"."taxon"."taxonomicStatus" IS 'The status of the use of the scientificName as a label for a taxon. Requires taxonomic opinion to define the scope of a taxon. Rules of priority then are used to define the taxonomic status of the nomenclature contained in that scope, combined with the experts opinion. It must be linked to a specific taxonomic reference that defines the concept. Recommended best practice is to use a controlled vocabulary.';
COMMENT ON COLUMN "public"."taxon"."vernacularName" IS 'The status of the use of the scientificName as a label for a taxon. Requires taxonomic opinion to define the scope of a taxon. Rules of priority then are used to define the taxonomic status of the nomenclature contained in that scope, combined with the experts opinion. It must be linked to a specific taxonomic reference that defines the concept. Recommended best practice is to use a controlled vocabulary.';










CREATE TABLE "public"."extendedmeasurementorfact" (
    "eventID" text,
    "occurrenceID" text,
    "measurementID" text,
    "measurementType" text,
    "measurementTypeID" text,
    "measurementValue" text,
    "measurementValueID" text,
    "measurementUnit" text,
    "measurementUnitID" text,
    "measurementAccuracy" text,
    "measurementDate" text,
    "measurementDeterminedBy" text,
    "measurementRemarks" text,
);
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."eventID" IS 'An identifier for the set of information associated with an Event (something that occurs at a place and time).';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."occurrenceID" IS 'An identifier for the Occurrence (as opposed to a particular digital record of the occurrence). In the absence of a persistent global unique identifier, construct one from a combination of identifiers in the record that will most closely make the occurrenceID globally unique.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementID" IS 'An identifier for the MeasurementOrFact (information pertaining to measurements, facts, characteristics, or assertions). May be a global unique identifier or an identifier specific to the data set.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementType" IS 'The nature of the measurement, fact, characteristic, or assertion. Recommended best practice is to use a controlled vocabulary.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementTypeID" IS 'An identifier for the measurementType (global unique identifier, URI). The identifier should reference the measurementType in a vocabulary.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementValue" IS 'The value of the measurement, fact, characteristic, or assertion.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementValueID" IS 'An identifier for facts stored in the column measurementValue (global unique identifier, URI). This identifier can reference a controlled vocabulary (e.g. for sampling instrument names, methodologies, life stages) or reference a methodology paper with a DOI. When the measurementValue refers to a value and not to a fact, the measurementvalueID has no meaning and should remain empty.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementUnit" IS 'The units associated with the measurementValue. Recommended best practice is to use the International System of Units (SI). ';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementUnitID" IS 'An identifier for the measurementUnit (global unique identifier, URI). The identifier should reference the measurementUnit in a vocabulary.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementAccuracy" IS 'The description of the potential error associated with the measurementValue.';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementDate" IS 'The date on which the MeasurementOrFact was made. Recommended best practice is to use an encoding scheme, such as ISO 8601:2004(E).';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementDeterminedBy" IS 'A list (concatenated and separated) of names of people, groups, or organizations who determined the value of the MeasurementOrFact. ';
COMMENT ON COLUMN "public"."extendedmeasurementorfact"."measurementRemarks" IS 'Comments or notes accompanying the MeasurementOrFact. ';
